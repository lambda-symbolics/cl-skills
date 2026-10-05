(in-package #:cl-skills/tests)

(defvar *executable-call-count* 0
  "Calls executed by the invocation fixture.")

(defun executable-echo (input &key context)
  "Echo valid input and require the originating opaque context."
  (incf *executable-call-count*)
  (unless (eq context ':fixture) (error "Context lost."))
  input)

(defun executable-bad-output (input &key context)
  "Return an invalid output for contract failure tests."
  (declare (ignore input context))
  42)

(defun executable-error (input &key context)
  "Signal an entrypoint failure."
  (declare (ignore input context))
  (error "Fixture entrypoint failure."))

(defun executable-self-test (&key context)
  "Run a real callable self-test of the fixture entrypoint."
  (string= (executable-echo "self-test" :context context) "self-test"))

(defun executable-verification-fails (&key context)
  "Return a failed verification result."
  (declare (ignore context))
  nil)

(defun executable-verification-errors (&key context)
  "Signal an error in verification."
  (declare (ignore context))
  (error "Fixture verification error."))

(defun tests--executable-form (&rest fields)
  "Return a callable declaration, replacing defaults with FIELDS."
  (let ((manifest '(:format-version 1 :skill-version "1.0.0"
                   :entrypoint ("CL-SKILLS/TESTS" "EXECUTABLE-ECHO")
                   :self-test ("CL-SKILLS/TESTS" "EXECUTABLE-SELF-TEST")
                   :verify ("CL-SKILLS/TESTS" "EXECUTABLE-SELF-TEST")
                   :input (:type :string) :output (:type :string)
                   :capabilities ("read") :tools ("resource.read"))))
    (setf manifest (copy-tree manifest))
    (loop for (key value) on fields by #'cddr do (setf (getf manifest key) value))
    (prin1-to-string (cons ':skill-executable manifest))))

(defun tests--executable-skill (root &optional (source (tests--executable-form)))
  "Write a standard Skill and optional sidecar; return selected metadata."
  (tests--write root "echo/SKILL.md" (tests--standard "echo" "Echo input." "Echo."))
  (when source (tests--write root "echo/EXECUTABLE.sexp" source))
  (skill-catalog-find (skill-catalog-discover (list root)) "echo"))

(defun tests--executable-authorize (executable &key operation context)
  "Authorize only the fixture's explicit capability and tool declarations."
  (let ((manifest (skill-executable-manifest executable)))
    (and (eq context ':fixture)
         (member operation '(:invoke :self-test :verify))
         (equal (getf manifest :capabilities) '("read"))
         (equal (getf manifest :tools) '("resource.read"))
         t)))

(defun tests--executable-failure (kind function)
  "Require FUNCTION to signal an executable failure with KIND."
  (handler-case
      (progn (funcall function) (test-assert nil "Expected typed executable failure."))
    (skill-executable-error (condition)
      (test-assert (eq (skill-executable-error-kind condition) kind)
                   (format nil "Expected ~S, got ~S: ~A" kind (skill-executable-error-kind condition) condition))
      (test-assert (pathnamep (skill-executable-error-pathname condition)) "Failure retains sidecar pathname."))))

(defun test-executable-discovery ()
  "Exercise optional data-only discovery and fresh metadata copies."
  (with-test-root (root)
    (let ((skill (tests--executable-skill root nil)))
      (test-assert (null (skill-executable-discover skill)) "Absent sidecar is optional.")
      (tests--write root "echo/EXECUTABLE.sexp"
                    (tests--executable-form :system "cl-skills-no-such-system"
                                            :entrypoint '("NO-SUCH-PACKAGE" "NO-SUCH-FUNCTION")))
      (let* ((executable (skill-executable-discover skill))
             (manifest (skill-executable-manifest executable)))
        (test-assert (typep executable 'skill-executable) "Discover without system or symbol resolution.")
        (test-assert (= 64 (length (skill-executable-digest executable))) "Content identity is SHA-256.")
        (test-assert (null (find-package "NO-SUCH-PACKAGE")) "Discovery does not create entrypoint packages.")
        (setf (char (getf manifest :skill-version) 0) #\9)
        (test-assert (string= "1.0.0" (getf (skill-executable-manifest executable) :skill-version))
                     "Returned manifest strings are independent."))
      (tests--write root "echo/EXECUTABLE.sexp"
                    "(:skill-executable :format-version 1 :skill-version \"1\" :system \"only-system\")")
      (test-assert (skill-executable-discover skill) "System-only manifests are valid metadata."))))

(defun test-executable-rejections ()
  "Exercise safe readers, exact fields, versions, contracts and resource bounds."
  (with-test-root (root)
    (let ((skill (tests--executable-skill root nil)))
      (dolist (case (list
                    (list ':invalid-syntax "#.(error \"reader evaluation\")")
                    (list ':multiple-forms "(:skill-executable) (:skill-executable)")
                    (list ':invalid-structure "(:skill-executable :format-version . 1)")
                    (list ':duplicate-field "(:skill-executable :format-version 1 :format-version 1)")
                    (list ':unknown-field "(:skill-executable :type :string)")
                    (list ':invalid-version (tests--executable-form :format-version 2))
                    (list ':invalid-version (tests--executable-form :skill-version ""))
                    (list ':invalid-entrypoint (tests--executable-form :entrypoint '("CL-SKILLS/TESTS")))
                    (list ':invalid-contract (tests--executable-form :input '(:type :object :required ("missing"))))
                    (list ':invalid-contract (tests--executable-form :output '(:type :array :items (:type :string)
                                                                                     :min-items 2 :max-items 1)))
                    (list ':invalid-structure (tests--executable-form :capabilities '("read" "read")))
                    (list ':invalid-structure (tests--executable-form :dependencies '(("same" "1") ("same" "2"))))
                    (list ':invalid-structure (tests--executable-form :system-version "1"))))
        (tests--write root "echo/EXECUTABLE.sexp" (second case))
        (tests--executable-failure (first case) (lambda () (skill-executable-discover skill))))
      (tests--write root "echo/EXECUTABLE.sexp" (tests--executable-form))
      (let ((*skill-executable-character-limit* 20))
        (tests--executable-failure ':file-too-large (lambda () (skill-executable-discover skill))))
      (let ((*skill-executable-node-limit* 8))
        (tests--executable-failure ':data-too-large (lambda () (skill-executable-discover skill))))
      (let ((*skill-executable-depth-limit* 1))
        (tests--executable-failure ':data-too-deep (lambda () (skill-executable-discover skill)))))))

(defun test-executable-invocation ()
  "Exercise authority, actual calls, output contracts and callable self-tests."
  (with-test-root (root)
    (let* ((executable (skill-executable-discover (tests--executable-skill root)))
           (*executable-call-count* 0))
      (tests--executable-failure ':not-authorized (lambda () (skill-executable-invoke executable "ok")))
      (test-assert (zerop *executable-call-count*) "Denied invocation executes nothing.")
      (tests--executable-failure ':invalid-input
                                (lambda () (skill-executable-invoke executable 42
                                           :authorize #'tests--executable-authorize :context ':fixture)))
      (test-assert (string= "Unicode λ: 日本語"
                           (skill-executable-invoke executable "Unicode λ: 日本語"
                            :authorize #'tests--executable-authorize :context ':fixture))
                   "Actual callable entrypoint preserves UTF-8 input.")
      (test-assert (= 1 *executable-call-count*) "One real invocation.")
      (test-assert (skill-executable-verify executable :authorize #'tests--executable-authorize :context ':fixture)
                   "Actual callable self-test passes.")
      (test-assert (skill-executable-verify executable :entrypoint ':verify
                    :authorize #'tests--executable-authorize :context ':fixture) "Verification entrypoint passes.")
      (test-assert (= 3 *executable-call-count*) "Both self-tests exercised the function.")
      (tests--executable-failure ':authorization-error
                                (lambda () (skill-executable-invoke executable "ok"
                                           :authorize (lambda (&rest arguments) (declare (ignore arguments)) (error "Denied.")))))
      (tests--executable-failure ':cancelled
                                (lambda () (skill-executable-invoke executable "ok" :cancelled-p (constantly t))))
      (test-assert (= 3 *executable-call-count*) "Pre-cancelled request executes nothing.")
      (let ((input (list ':array)))
        (setf (rest input) input)
        (tests--executable-failure ':invalid-input
                                  (lambda () (skill-executable-invoke executable input))))
      (let ((*skill-executable-character-limit* 2))
        (tests--executable-failure ':invalid-input
                                  (lambda () (skill-executable-invoke executable "oversized")))))
    (dolist (case '((:invalid-output "EXECUTABLE-BAD-OUTPUT")
                    (:invocation-error "EXECUTABLE-ERROR")
                    (:missing-entrypoint "NO-SUCH-FUNCTION")))
      (let ((executable (skill-executable-discover
                         (tests--executable-skill root
                           (tests--executable-form :entrypoint (list "CL-SKILLS/TESTS" (second case)))))))
        (tests--executable-failure (first case)
                                  (lambda () (skill-executable-invoke executable "ok"
                                             :authorize #'tests--executable-authorize :context ':fixture)))))
    (dolist (case '((:verification-failed "EXECUTABLE-VERIFICATION-FAILS")
                    (:verification-error "EXECUTABLE-VERIFICATION-ERRORS")
                    (:missing-entrypoint "NO-SUCH-FUNCTION")))
      (let ((executable (skill-executable-discover
                         (tests--executable-skill root
                           (tests--executable-form :self-test (list "CL-SKILLS/TESTS" (second case)))))))
        (tests--executable-failure (first case)
                                  (lambda () (skill-executable-verify executable
                                           :authorize #'tests--executable-authorize :context ':fixture)))))))

(defun test-executable-identity ()
  "Reject changed content, replacements and changes made by authorization."
  (with-test-root (root)
    (let* ((skill (tests--executable-skill root))
           (executable (skill-executable-discover skill)))
      (tests--write root "echo/EXECUTABLE.sexp" (tests--executable-form :skill-version "2"))
      (tests--executable-failure ':identity-changed
                                (lambda () (skill-executable-invoke executable "ok"
                                           :authorize #'tests--executable-authorize :context ':fixture)))
      (setf executable (skill-executable-discover skill))
      (rename-file (merge-pathnames "echo/EXECUTABLE.sexp" root)
                   (merge-pathnames "echo/previous.sexp" root))
      (tests--write root "echo/EXECUTABLE.sexp" (tests--executable-form :skill-version "2"))
      (tests--executable-failure ':identity-changed
                                (lambda () (skill-executable-verify executable
                                           :authorize #'tests--executable-authorize :context ':fixture)))
      (setf executable (skill-executable-discover skill))
      (tests--executable-failure ':identity-changed
                                (lambda () (skill-executable-invoke executable "ok"
                                           :authorize (lambda (&rest arguments)
                                                        (declare (ignore arguments))
                                                        (tests--write root "echo/EXECUTABLE.sexp" (tests--executable-form))
                                                        t)))))))

(defun test-executable-systems ()
  "Load a real ASDF fixture and verify version checks precede code loading."
  (with-test-root (root)
    (let* ((system-name (format nil "cl-skills-executable-fixture-~D" (random most-positive-fixnum)))
           (asd (tests--write root (concatenate 'string system-name ".asd")
                  (format nil "(asdf:defsystem ~S :version \"1.2.0\" :components ((:file \"callable\")))" system-name)))
           (loaded nil) (authorizations 0))
      (tests--write root "callable.lisp"
        "(in-package #:cl-skills/tests)
(defun loaded-executable (input &key context)
  (declare (ignore context))
  (concatenate 'string input \"-loaded\"))
(defun loaded-self-test (&key context)
  (string= (loaded-executable \"test\" :context context) \"test-loaded\"))")
      (asdf:load-asd asd)
      (labels ((authorize (&rest arguments)
                 (incf authorizations)
                 (apply #'tests--executable-authorize arguments))

               (loader (name &key context)
                 (test-assert (eq context ':fixture) "Loader receives caller context.")
                 (test-assert (string= name system-name) "Loader receives declared system.")
                 (setf loaded t)
                 (asdf:load-system name)))
        (unwind-protect
             (let ((executable (skill-executable-discover
                                (tests--executable-skill root
                                  (tests--executable-form :system system-name :system-version "2.0.0"
                                    :entrypoint '("CL-SKILLS/TESTS" "LOADED-EXECUTABLE")
                                    :self-test '("CL-SKILLS/TESTS" "LOADED-SELF-TEST"))))))
               (test-assert (null loaded) "Discovery executes no loader.")
               (tests--executable-failure ':loader-required
                 (lambda () (skill-executable-invoke executable "test" :authorize #'authorize :context ':fixture)))
               (tests--executable-failure ':version-mismatch
                 (lambda () (skill-executable-invoke executable "test" :authorize #'authorize
                            :loader #'loader :context ':fixture)))
               (test-assert (null loaded) "Version mismatch precedes loading.")
               (setf executable (skill-executable-discover
                                 (tests--executable-skill root
                                   (tests--executable-form :system system-name :system-version "1.2.0"
                                    :dependencies (list (list system-name "1.0.0"))
                                    :entrypoint '("CL-SKILLS/TESTS" "LOADED-EXECUTABLE")
                                    :self-test '("CL-SKILLS/TESTS" "LOADED-SELF-TEST")))))
               (test-assert (string= "test-loaded" (skill-executable-invoke executable "test"
                            :authorize #'authorize :loader #'loader :context ':fixture)) "Real ASDF callable invocation.")
               (test-assert (skill-executable-verify executable :authorize #'authorize :loader #'loader :context ':fixture)
                            "Real loaded ASDF callable self-test.")
               (test-assert (= authorizations 4) "Every invocation separately authorized."))
          (asdf:clear-system system-name)
          (dolist (name '("LOADED-EXECUTABLE" "LOADED-SELF-TEST"))
            (let ((symbol (find-symbol name '#:cl-skills/tests)))
              (when (and symbol (fboundp symbol)) (fmakunbound symbol)))))))))

(defun test-executable-boundaries ()
  "Exercise contract transport, strict authority, cancellation and loader failures."
  (with-test-root (root)
    (let* ((schema '(:type :object :properties (("items" (:type :array :items (:type :boolean))))
                    :required ("items") :additional-properties nil))
           (executable (skill-executable-discover
                        (tests--executable-skill root (tests--executable-form :input schema :output schema))))
           (input '(:object ("items" (:array t nil))))
           (*executable-call-count* 0))
      (test-assert (equal input (skill-executable-invoke executable input
                                :authorize #'tests--executable-authorize :context ':fixture))
                   "Nested tagged objects, arrays and false satisfy shared contracts.")
      (tests--executable-failure ':invalid-input
        (lambda () (skill-executable-invoke executable '(:object ("extra" 1)))))
      (tests--executable-failure ':invalid-input
        (lambda () (skill-executable-invoke executable '(:object ("items" (:array)) ("items" (:array))))))
      (tests--executable-failure ':not-authorized
        (lambda () (skill-executable-invoke executable input :authorize (constantly ':yes))))
      (tests--executable-failure ':cancellation-error
        (lambda () (skill-executable-invoke executable input
                   :cancelled-p (lambda () (error "Cancellation predicate failed.")))))
      (tests--executable-failure ':cancelled
        (lambda () (skill-executable-invoke executable input
                   :authorize #'tests--executable-authorize :context ':fixture
                   :cancelled-p (lambda () (> *executable-call-count* 1)))))
      (test-assert (= *executable-call-count* 2) "Post-call cancellation rejects a completed result."))
    (dolist (case (list
                   (list ':missing-system :system "no-such-cl-skills-system")
                   (list ':version-mismatch :dependencies '(("cl-skills" "99.0.0")))
                   (list ':load-error :system "cl-skills")))
      (let ((executable (skill-executable-discover
                         (tests--executable-skill root (apply #'tests--executable-form (rest case)))))
            (load-count 0))
        (tests--executable-failure (first case)
          (lambda () (skill-executable-invoke executable "ok"
                     :authorize #'tests--executable-authorize :context ':fixture
                     :loader (lambda (&rest arguments) (declare (ignore arguments))
                               (incf load-count) (error "Loader failed.")))))
        (test-assert (= load-count (if (eq (first case) ':load-error) 1 0))
                     "Only a valid declaration reaches the caller loader.")))
    (dolist (version '("invalid" "1..2" ".1" "1." "1-2"))
      (let ((skill (tests--executable-skill root (tests--executable-form :system "cl-skills" :system-version version))))
        (tests--executable-failure ':invalid-version (lambda () (skill-executable-discover skill)))))
    (let ((skill (tests--executable-skill root (tests--executable-form :entrypoint '("CL" "WHEN")))))
      (tests--executable-failure ':missing-entrypoint
        (lambda () (skill-executable-invoke (skill-executable-discover skill) "ok"
                   :authorize #'tests--executable-authorize :context ':fixture))))))

(defun test-executable-confinement ()
  "Reject outside-root links and nonregular optional sidecars without blocking."
  (with-test-root (root)
    (with-test-root (outside)
      (let* ((skill (tests--executable-skill root nil))
             (sidecar (merge-pathnames "echo/EXECUTABLE.sexp" root))
             (external (tests--write outside "external.sexp" (tests--executable-form))))
        (uiop:run-program (list "ln" "-s" (namestring external) (namestring sidecar)))
        (tests--executable-failure ':outside-root (lambda () (skill-executable-discover skill)))
        (delete-file sidecar)
        (uiop:run-program (list "mkfifo" (namestring sidecar)))
        (tests--executable-failure ':not-regular-file (lambda () (skill-executable-discover skill)))
        (delete-file sidecar)
        (ensure-directories-exist (uiop:ensure-directory-pathname sidecar))
        (tests--executable-failure ':not-regular-file (lambda () (skill-executable-discover skill)))))))
(defun run-executable-tests ()
  "Run optional executable Skill behavior and actual callable tests."
  (let ((*test-count* 0))
    (mapc #'funcall '(test-executable-discovery test-executable-rejections
                     test-executable-invocation test-executable-identity test-executable-systems
                     test-executable-boundaries test-executable-confinement))
    (format t "~D executable cl-skills assertions passed.~%" *test-count*)
    t))
