(defpackage #:cl-skills/tests
  (:use #:cl #:cl-skills))

(in-package #:cl-skills/tests)

(defvar *test-count* 0
  "The number of assertions executed by the current test run.")

(defun test-assert (condition description)
  "Count and require CONDITION, reporting DESCRIPTION on failure."
  (incf *test-count*)
  (unless condition
    (error "Test failed: ~A" description))
  t)

(defun tests--temporary-directory ()
  "Create and return one unique temporary directory."
  (let ((directory
          (merge-pathnames
           (format nil "cl-skills-tests-~D-~D/"
                   (get-universal-time)
                   (random most-positive-fixnum))
           (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "placeholder" directory))
    directory))

(defmacro with-test-root ((root) &body body)
  "Evaluate BODY with ROOT bound to a fresh temporary directory."
  `(let ((,root (tests--temporary-directory)))
     (unwind-protect
          (progn ,@body)
       (when (probe-file ,root)
         (uiop:delete-directory-tree ,root
                                     :validate t
                                     :if-does-not-exist ':ignore)))))

(defun tests--write (root relative content)
  "Write CONTENT beneath ROOT at RELATIVE and return its pathname."
  (let ((pathname (merge-pathnames relative root)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction ':output
                            :if-exists ':supersede
                            :if-does-not-exist ':create
                            :external-format ':utf-8)
      (write-string content stream))
    pathname))

(defun tests--write-native-entry (directory name content)
  "Write CONTENT to literal native NAME beneath DIRECTORY."
  (let ((pathname
          (sb-ext:parse-native-namestring
           (concatenate 'string
                        (sb-ext:native-namestring directory)
                        name))))
    (with-open-file (stream pathname
                            :direction ':output
                            :if-exists ':supersede
                            :if-does-not-exist ':create
                            :external-format ':utf-8)
      (write-string content stream))
    pathname))

(defun tests--native (name description instructions &key (version 1))
  "Return one native skill definition."
  (format nil
          "(:autolith-skill~% :version ~S~% :name ~S~% :description ~S~% :instructions ~S)~%"
          version name description instructions))

(defun tests--standard (name description instructions)
  "Return one standard Agent Skill definition."
  (format nil
          "---~%name: ~S~%description: ~S~%---~%~A"
          name description instructions))

(defun tests--names (catalog)
  "Return selected skill names from CATALOG."
  (mapcar #'skill-metadata-name (skill-catalog-skills catalog)))

(defun tests--kinds (catalog)
  "Return diagnostic kinds from CATALOG."
  (mapcar #'skill-diagnostic-kind (skill-catalog-diagnostics catalog)))

(defun tests--manifest-pathname (cache-pathname)
  "Return CACHE-PATHNAME's manifest pathname."
  (merge-pathnames "manifest.sha256"
                   (uiop:pathname-directory-pathname cache-pathname)))

(defun test-path-classification ()
  "Test exact case-sensitive source classification."
  (test-assert
   (eq (skill-source-format-for-pathname #P"SKILL.sexp") ':native)
   "SKILL.sexp is native")
  (test-assert
   (eq (skill-source-format-for-pathname #P"SKILL.md") ':agent-skill)
   "SKILL.md is standard")
  (dolist (pathname '(#P"skill.md" #P"Skill.md" #P"SKILL.MD"
                      #P"SKILL.sexp.bak" #P"NOTES.md"))
    (test-assert
     (null (skill-source-format-for-pathname pathname))
     (format nil "~A is not a supported exact filename" pathname)))
  (test-assert (skill-source-pathname-p #P"SKILL.md")
               "supported pathname predicate accepts SKILL.md")
  (test-assert (not (skill-source-pathname-p #P"skill.md"))
               "supported pathname predicate rejects wrong case"))

(defun test-discovery-and-precedence ()
  "Test deterministic traversal, parsing, precedence, and fresh reads."
  (with-test-root (root)
    (let ((primary (merge-pathnames "primary/" root))
          (secondary (merge-pathnames "secondary/" root)))
      (tests--write primary "standard/SKILL.md"
                    (tests--standard "standard" "Standard skill." "Body B\n"))
      (tests--write primary "a/SKILL.sexp"
                    (tests--native "alpha" (format nil "Handles~%  related work.") "Body A"))
      (tests--write primary "same/SKILL.md"
                    (tests--standard "same"
                                     "Standard sibling." "Standard body"))
      (tests--write primary "same/SKILL.sexp"
                    (tests--native "same" "Native sibling." "Native body"))
      (tests--write primary "bad/SKILL.sexp"
                    "(:autolith-skill :version 1 :name \"bad\")")
      (tests--write primary "wrong/skill.md"
                    (tests--standard "wrong-case" "Ignored." "Ignored"))
      (tests--write secondary "a/SKILL.sexp"
                    (tests--native "alpha" "Loses." "Secondary"))
      (tests--write secondary "z/SKILL.sexp"
                    (tests--native "zeta" "Last." "Body Z"))
      (let* ((catalog (skill-catalog-discover (list primary secondary)))
             (kinds (tests--kinds catalog))
             (alpha (skill-catalog-find catalog "alpha")))
        (test-assert
         (equal (tests--names catalog)
                '("alpha" "same" "standard" "zeta"))
         "catalog order is deterministic and earlier roots win")
        (test-assert
         (string= (skill-metadata-description alpha) "Handles related work.")
         "descriptions are normalized for metadata")
        (test-assert
         (eq (skill-metadata-source-format alpha) ':native)
         "metadata records source format")
        (test-assert
         (uiop:pathname-equal (skill-metadata-root alpha)
                              (uiop:ensure-directory-pathname primary))
         "metadata records discovery root")
        (test-assert (= (skill-metadata-root-index alpha) 0)
                     "metadata records root precedence")
        (test-assert (member ':missing-field kinds)
                     "malformed source produces typed diagnostic")
        (test-assert (= (count ':shadowed kinds) 2)
                     "later duplicate name is diagnosed as shadowed")
        (test-assert (null (skill-catalog-find catalog "wrong-case"))
                     "wrong-case source filename is ignored")
        (test-assert (string= (skill-metadata-read alpha) "Body A")
                     "selected metadata rereads instructions")
        (tests--write primary "a/SKILL.sexp"
                      (tests--native "alpha" "Changed." "Fresh body"))
        (test-assert (string= (skill-metadata-read alpha) "Fresh body")
                     "metadata reads the current source rather than retained body")))))

(defun test-native-validation ()
  "Test native syntax, shape, bounds, and reader safety."
  (with-test-root (root)
    (let ((cases
            `((:missing-field
               "(:autolith-skill :version 1 :name \"x\" :description \"d\")")
              (:unknown-field
               "(:autolith-skill :version 1 :name \"x\" :description \"d\" :instructions \"i\" :extra t)")
              (:duplicate-field
               "(:autolith-skill :version 1 :name \"x\" :name \"y\" :description \"d\" :instructions \"i\")")
              (:invalid-version
               ,(tests--native "x" "d" "i" :version 2))
              (:invalid-name
               "(:autolith-skill :version 1 :name 1 :description \"d\" :instructions \"i\")")
              (:invalid-description
               "(:autolith-skill :version 1 :name \"x\" :description \" \" :instructions \"i\")")
              (:invalid-instructions
               "(:autolith-skill :version 1 :name \"x\" :description \"d\" :instructions \" \" )")
              (:invalid-structure
               "42")
              (:invalid-syntax
               "#.(error \"reader evaluation escaped\")")
              (:invalid-syntax
               "(:autolith-skill :version 1 :name \"x\" :description \"d\" :instructions \"i\") extra"))))
      (loop for (expected source) in cases
            for index from 0
            do (tests--write root (format nil "case-~D/SKILL.sexp" index) source))
      (let ((kinds (tests--kinds (skill-catalog-discover (list root)))))
        (dolist (case cases)
          (test-assert
           (member (first case) kinds)
           (format nil "native rejection reports ~S" (first case))))))
    (let ((*skill-form-depth-limit* 5))
      (tests--write root "deep/SKILL.sexp"
                    "(:autolith-skill :version 1 :name \"deep\" :description \"d\" :instructions (((((\"i\"))))))")
      (test-assert
       (member ':data-too-deep
               (tests--kinds (skill-catalog-discover (list root))))
       "native structural depth is bounded"))
    (let ((*skill-form-node-limit* 6))
      (tests--write root "nodes/SKILL.sexp"
                    (tests--native "nodes" "d" "i"))
      (test-assert
       (member ':data-too-large
               (tests--kinds (skill-catalog-discover (list root))))
       "native structural node count is bounded"))
    (let ((*skill-file-character-limit* 40))
      (tests--write root "large/SKILL.sexp"
                    (tests--native "large" "description" "instructions"))
      (test-assert
       (member ':file-too-large
               (tests--kinds (skill-catalog-discover (list root))))
       "native source character count is bounded"))))

(defun test-filesystem-boundaries ()
  "Test root confinement and non-regular source handling."
  (with-test-root (root)
    (with-test-root (outside)
      (let* ((target (tests--write outside "SKILL.sexp"
                                  (tests--native "outside" "Outside." "Body")))
             (link (merge-pathnames "linked/SKILL.sexp" root))
             (fifo (merge-pathnames "fifo/SKILL.sexp" root)))
        (ensure-directories-exist link)
        (sb-posix:symlink (namestring target) (namestring link))
        (ensure-directories-exist fifo)
        (sb-posix:mkfifo (namestring fifo) #o600)
        (let ((kinds (tests--kinds (skill-catalog-discover (list root)))))
          (test-assert (member ':outside-root kinds)
                       "source symlinks cannot escape their canonical root")
          (test-assert (member ':not-regular-file kinds)
                       "non-regular exact source candidates fail closed"))))
    (let* ((broken (merge-pathnames "broken/SKILL.md" root))
           (missing (merge-pathnames "missing-target" root)))
      (ensure-directories-exist broken)
      (sb-posix:symlink (namestring missing) (namestring broken))
      (test-assert
       (member ':read-error
               (tests--kinds (skill-catalog-discover (list root))))
       "unresolved source links produce scan diagnostics"))))

(defun test-canonical-discovery-roots ()
  "Test root aliases, component boundaries, and missing discovery roots."
  (with-test-root (parent)
    (let* ((root (merge-pathnames "skills/" parent))
           (sibling (merge-pathnames "skills-extra/" parent))
           (alias (merge-pathnames "alias" parent))
           (missing (merge-pathnames "missing/" parent))
           (source (tests--write root "SKILL.sexp"
                                 (tests--native "inside" "Inside." "Inside body")))
           (outside (tests--write sibling "SKILL.sexp"
                                  (tests--native "outside" "Outside." "Outside body")))
           (link (merge-pathnames "linked/SKILL.sexp" root)))
      (ensure-directories-exist link)
      (sb-posix:symlink (sb-ext:native-namestring outside)
                        (sb-ext:native-namestring link))
      (sb-posix:symlink (sb-ext:native-namestring root)
                        (sb-ext:native-namestring alias))
      (dolist (discovery-root (list root (uiop:ensure-directory-pathname alias)))
        (let* ((catalog (skill-catalog-discover (list discovery-root missing)))
               (metadata (skill-catalog-find catalog "inside"))
               (kinds (tests--kinds catalog)))
          (test-assert (equal (tests--names catalog) '("inside"))
                       "a canonical or aliased root admits only its own source")
          (test-assert
           (uiop:pathname-equal (skill-metadata-canonical-pathname metadata)
                                (ls-compat.posix:canonical-pathname source))
           "metadata records the canonical source behind a root alias")
          (test-assert (string= (skill-metadata-read metadata) "Inside body")
                       "a source at the discovery root can be read freshly")
          (test-assert (member ':outside-root kinds)
                       "a similarly prefixed sibling is outside the root")
          (test-assert (member ':missing-root kinds)
                       "a missing root is diagnosed without blocking a valid root"))))))

(defun test-literal-filesystem-entry-names ()
  "Test that host-valid names cannot abort discovery pathname construction."
  (with-test-root (root)
    (tests--write-native-entry
     root
     "[English auto-generated [Do.txt"
     "This unrelated file has a literal unmatched bracket in its name.")
    (tests--write root "valid/SKILL.sexp"
                  (tests--native "valid" "Valid." "Body"))
    (let ((catalog (skill-catalog-discover (list root))))
      (test-assert (skill-catalog-find catalog "valid")
                   "an unmatched bracket in a sibling filename does not abort discovery")
      (test-assert (not (member ':scan-error (tests--kinds catalog)))
                   "literal host filenames do not produce pathname parse diagnostics"))))

(defun test-symlinked-skill-directories ()
  "Test symlinked directories across configured roots and fresh-read confinement."
  (with-test-root (primary)
    (with-test-root (secondary)
      (with-test-root (outside)
        (let* ((target-directory (merge-pathnames "shared/" secondary))
               (outside-directory (merge-pathnames "escaped/" outside))
               (link (merge-pathnames "linked" primary)))
          (tests--write target-directory "SKILL.sexp"
                        (tests--native "linked" "Linked." "Linked body"))
          (tests--write outside-directory "SKILL.sexp"
                        (tests--native "escaped" "Escaped." "Outside body"))
          (sb-posix:symlink (sb-ext:native-namestring target-directory)
                            (sb-ext:native-namestring link))
          (let* ((catalog (skill-catalog-discover (list primary secondary)))
                 (metadata (skill-catalog-find catalog "linked")))
            (test-assert metadata
                         "a skill directory may link into another configured root")
            (test-assert (uiop:pathname-equal
                          (skill-metadata-root metadata)
                          primary)
                         "symlinked metadata retains the discovery root")
            (test-assert (string= (skill-metadata-read metadata) "Linked body")
                         "symlinked skill instructions can be read freshly")
            (sb-posix:unlink (sb-ext:native-namestring link))
            (sb-posix:symlink (sb-ext:native-namestring outside-directory)
                              (sb-ext:native-namestring link))
            (let ((condition nil))
              (handler-case
                  (skill-metadata-read metadata)
                (skill-read-error (read-error)
                  (setf condition read-error)))
              (test-assert condition
                           "retargeting a selected directory outside configured roots fails closed"))
            (test-assert
             (member ':outside-root
                     (tests--kinds
                      (skill-catalog-discover (list primary secondary))))
             "discovery does not follow a directory outside configured roots")))))))

(defun test-scan-limits ()
  "Test bounded traversal and aggregate character budgets."
  (with-test-root (root)
    (tests--write root "a/b/c/SKILL.sexp"
                  (tests--native "deep" "Deep." "Body"))
    (test-assert
     (member ':scan-depth-limit
             (tests--kinds
              (skill-catalog-discover (list root) :max-depth 1)))
     "directory depth is bounded")
    (dotimes (index 6)
      (tests--write root (format nil "many/~D/SKILL.sexp" index)
                    (tests--native (format nil "s~D" index) "Skill." "Body")))
    (test-assert
     (member ':scan-directory-limit
             (tests--kinds
              (skill-catalog-discover (list root) :max-directories 2)))
     "directory count is bounded")
    (test-assert
     (member ':scan-entry-limit
             (tests--kinds
              (skill-catalog-discover (list root) :max-entries 2)))
     "entry count is bounded")
    (test-assert
     (member ':scan-character-limit
             (tests--kinds
              (skill-catalog-discover (list root) :max-characters 20)))
     "aggregate source characters are bounded")))

(defun test-standard-validation ()
  "Test strict YAML frontmatter, names, fields, and exact bodies."
  (with-test-root (root)
    (let ((body (format nil "# Instructions~2%Preserve  two spaces.~%")))
      (tests--write root "valid-skill/SKILL.md"
                    (format nil
                            "---~%name: valid-skill~%description: A valid standard skill.~%license: COLL-Attribution~%compatibility: SBCL~%metadata:~%  owner: lambda-symbolics~%allowed-tools: read write~%---~%~A"
                            body))
      (let* ((catalog (skill-catalog-discover (list root)))
             (metadata (skill-catalog-find catalog "valid-skill")))
        (test-assert metadata "valid standard Skill is discovered")
        (test-assert (eq (skill-metadata-source-format metadata) ':agent-skill)
                     "standard metadata records its source format")
        (test-assert (string= (skill-metadata-read metadata) body)
                     "standard Markdown body is preserved exactly")))
    (let ((cases
            `((:invalid-syntax "absent/SKILL.md"
               ,(format nil "name: absent-boundaries~%description: bad~%"))
              (:invalid-syntax "unclosed/SKILL.md"
               ,(format nil "---~%name: unclosed~%description: bad~%"))
              (:unknown-field "unknown/SKILL.md"
               ,(format nil "---~%name: unknown~%description: bad~%surprise: true~%---~%Body"))
              (:missing-field "no-name/SKILL.md"
               ,(format nil "---~%description: no-name~%---~%Body"))
              (:invalid-name "uppercase/SKILL.md"
               ,(format nil "---~%name: Uppercase~%description: bad~%---~%Body"))
              (:invalid-name "leading/SKILL.md"
               ,(format nil "---~%name: -leading~%description: bad~%---~%Body"))
              (:invalid-name "trailing/SKILL.md"
               ,(format nil "---~%name: trailing-~%description: bad~%---~%Body"))
              (:invalid-name "two-hyphens/SKILL.md"
               ,(format nil "---~%name: two--hyphens~%description: bad~%---~%Body"))
              (:invalid-description "empty-description/SKILL.md"
               ,(format nil "---~%name: empty-description~%description: '  '~%---~%Body"))
              (:accepted-empty-body "empty-body/SKILL.md"
               ,(format nil "---~%name: empty-body~%description: Empty body~%---~%")))))
      (loop for (expected relative source) in cases
            do (tests--write root relative source))
      (let ((kinds (tests--kinds (skill-catalog-discover (list root)))))
        (dolist (case cases)
          (unless (eq (first case) :accepted-empty-body)
            (test-assert
             (member (first case) kinds)
             (format nil "standard rejection reports ~S" (first case)))))
        (let ((empty (skill-catalog-find
                      (skill-catalog-discover (list root))
                      "empty-body")))
          (test-assert (and empty (string= (skill-metadata-read empty) ""))
                       "standard Agent Skills may have an empty body"))))
    (let ((*skill-description-character-limit* 4))
      (tests--write root "description-limit/SKILL.md"
                    (tests--standard "description-limit" "Too long" "Body"))
      (test-assert
       (member ':invalid-description
               (tests--kinds (skill-catalog-discover (list root))))
       "standard description length is bounded"))
    (let ((*skill-instruction-character-limit* 4))
      (tests--write root "instruction-limit/SKILL.md"
                    (tests--standard "instruction-limit" "Valid" "12345"))
      (let* ((catalog (skill-catalog-discover (list root)))
             (metadata (skill-catalog-find catalog "instruction-limit"))
             (bounded-p nil))
        (handler-case
            (skill-metadata-read metadata)
          (skill-body-too-large ()
            (setf bounded-p t)))
        (test-assert bounded-p
                     "selected standard instruction length is bounded")))))

(defun test-conversion-cache ()
  "Test content addressing, manifests, integrity checks, and regeneration."
  (with-test-root (root)
    (with-test-root (cache-root)
      (let* ((source (tests--standard "cached" "Cached skill." (format nil "Initial body~%")))
             (digest (skill-source-digest source))
             (expected-cache (skill-standard-cache-pathname cache-root digest))
             (catalog (progn
                        (tests--write root "cached/SKILL.md" source)
                        (skill-catalog-discover (list root) :cache-root cache-root)))
             (metadata (skill-catalog-find catalog "cached")))
        (test-assert (= (length digest) 64) "source digest is lowercase SHA-256")
        (test-assert (every (lambda (character)
                              (or (digit-char-p character)
                                  (find character "abcdef")))
                            digest)
                     "source digest uses lowercase hexadecimal")
        (test-assert (probe-file expected-cache)
                     "discovery creates content-addressed native cache")
        (test-assert (probe-file (tests--manifest-pathname expected-cache))
                     "discovery creates cache integrity manifest")
        (test-assert
         (uiop:pathname-equal (skill-metadata-cache-root metadata) cache-root)
         "standard metadata retains its cache root")
        (test-assert (string= (skill-metadata-read metadata) (format nil "Initial body~%"))
                     "cached standard instructions read correctly")
        (let ((cache-source (uiop:read-file-string expected-cache))
              (manifest (uiop:read-file-string
                         (tests--manifest-pathname expected-cache))))
          (test-assert (search ":autolith-skill" cache-source)
                       "conversion cache contains the native representation")
          (test-assert (search (format nil "source ~A" digest) manifest)
                       "manifest binds cache to source digest")
          (test-assert (search "cache " manifest)
                       "manifest records generated cache digest"))
        (tests--write (uiop:pathname-directory-pathname expected-cache)
                      "SKILL.sexp"
                      (tests--native "tampered" "Tampered." "Wrong"))
        (let* ((corrupt-inode
                 (sb-posix:stat-ino
                  (sb-posix:stat (namestring expected-cache))))
               (reloaded (skill-catalog-discover (list root) :cache-root cache-root))
               (fresh (skill-catalog-find reloaded "cached")))
          (test-assert fresh "corrupt cache regenerates from source")
          (test-assert (string= (skill-metadata-read fresh) (format nil "Initial body~%"))
                       "regenerated cache returns original instructions")
          (test-assert (search "\"cached\""
                               (uiop:read-file-string expected-cache))
                       "regeneration replaces corrupt native cache")
          (test-assert
           (/= corrupt-inode
               (sb-posix:stat-ino
                (sb-posix:stat (namestring expected-cache))))
           "corrupt cache regeneration atomically replaces its inode"))
        (let* ((changed (tests--standard "cached" "Cached skill." "Changed body"))
               (changed-digest (skill-source-digest changed))
               (changed-cache
                 (skill-standard-cache-pathname cache-root changed-digest)))
          (tests--write root "cached/SKILL.md" changed)
          (test-assert (string= (skill-metadata-read metadata) "Changed body")
                       "existing metadata rereads changed standard source")
          (let* ((changed-catalog
                   (skill-catalog-discover (list root) :cache-root cache-root))
                 (changed-metadata
                   (skill-catalog-find changed-catalog "cached")))
            (test-assert (probe-file changed-cache)
                         "source changes create a distinct cache directory")
            (test-assert (not (uiop:pathname-equal expected-cache changed-cache))
                         "cache pathname is content addressed")
            (test-assert (string= (skill-metadata-read changed-metadata)
                                  "Changed body")
                         "changed source reads through its new cache")))))))

(defun test-catalog-rendering ()
  "Test provider-neutral bounded catalog rendering."
  (with-test-root (root)
    (tests--write root "alpha/SKILL.sexp"
                  (tests--native "alpha" "A long useful description." "SECRET-A"))
    (tests--write root "beta/SKILL.sexp"
                  (tests--native "beta" "Another useful description." "SECRET-B"))
    (let ((catalog (skill-catalog-discover (list root))))
      (multiple-value-bind (rendered included omitted)
          (skill-catalog-render catalog)
        (test-assert (= included 2) "default catalog includes both skills")
        (test-assert (zerop omitted) "default catalog omits no skills")
        (test-assert (search "## Skills" rendered)
                     "catalog has provider-neutral heading")
        (test-assert (search "select that skill by exact name" rendered)
                     "catalog explains neutral host selection")
        (test-assert (and (search "alpha" rendered) (search "beta" rendered))
                     "catalog includes names")
        (test-assert (not (search "SECRET-A" rendered))
                     "catalog never retains native instructions")
        (test-assert (not (search "Autolith" rendered))
                     "catalog rendering is application neutral")
        (let ((prefix (format nil "CUSTOM PREFIX~%"))
              (guidance (format nil "~%CUSTOM GUIDANCE")))
          (multiple-value-bind
                (custom custom-included custom-omitted)
              (skill-catalog-render
               catalog
               :prefix prefix
               :guidance guidance)
            (test-assert
             (and
              (string= prefix (subseq custom 0 (length prefix)))
              (string=
               guidance
               (subseq custom (- (length custom) (length guidance))))
              (= custom-included included)
              (= custom-omitted omitted)
              (not (search "select that skill by exact name" custom)))
             "catalog rendering accepts exact host protocol sections")))
        (let ((bounded-result
                (loop for budget from 1 below (length rendered)
                      do (handler-case
                             (multiple-value-bind
                                   (bounded bounded-included bounded-omitted)
                                 (skill-catalog-render
                                  catalog
                                  :character-budget budget)
                               (when (plusp bounded-omitted)
                                 (return
                                   (list budget
                                         bounded
                                         bounded-included
                                         bounded-omitted))))
                           (skill-catalog-render-error ())))))
          (destructuring-bind
              (budget bounded bounded-included bounded-omitted)
              bounded-result
            (test-assert (<= (length bounded) budget)
                         "bounded catalog fits its character budget")
            (test-assert (= (+ bounded-included bounded-omitted) 2)
                         "bounded counts partition catalog metadata")
            (test-assert (plusp bounded-omitted)
                         "small catalog budget reports omissions"))))
      (let ((signaled nil))
        (handler-case
            (skill-catalog-render catalog :character-budget 10)
          (skill-catalog-render-error (condition)
            (setf signaled condition)))
        (test-assert signaled "impossibly small catalog budget is typed")
        (test-assert (= (skill-catalog-render-error-character-budget signaled) 10)
                     "render error records requested budget")
        (test-assert (> (skill-catalog-render-error-minimum-required signaled) 10)
                     "render error records required minimum")))
    (multiple-value-bind (rendered included omitted)
        (skill-catalog-render
         (make-instance 'skill-catalog :skills nil :diagnostics nil))
      (test-assert (search "No skills discovered" rendered)
                   "empty catalog renders explicitly")
      (test-assert (and (zerop included) (zerop omitted))
                   "empty catalog reports zero counts"))))

(defun test-read-failures ()
  "Test typed selected-source read failures."
  (with-test-root (root)
    (let* ((pathname (tests--write root "gone/SKILL.sexp"
                                  (tests--native "gone" "Temporary." "Body")))
           (metadata (skill-catalog-find
                      (skill-catalog-discover (list root)) "gone")))
      (delete-file pathname)
      (let ((condition nil))
        (handler-case
            (skill-metadata-read metadata)
          (skill-read-error (read-error)
            (setf condition read-error)))
        (test-assert condition "missing selected source signals skill-read-error")
        (test-assert (uiop:pathname-equal
                      (skill-read-error-pathname condition) pathname)
                     "read error records source pathname")
        (test-assert (skill-read-error-cause condition)
                     "read error retains underlying cause")))
    (let ((*skill-instruction-character-limit* 4))
      (tests--write root "large/SKILL.sexp"
                    (tests--native "large" "Large." "1234"))
      (let* ((metadata (skill-catalog-find
                        (skill-catalog-discover (list root)) "large")))
        (tests--write root "large/SKILL.sexp"
                      (tests--native "large" "Large." "12345"))
        (let ((condition nil))
          (handler-case
              (skill-metadata-read metadata)
            (skill-body-too-large (body-error)
              (setf condition body-error)))
          (test-assert condition "oversized selected body has a typed condition")
          (test-assert (= (skill-body-too-large-character-limit condition) 4)
                       "body-too-large records active instruction limit"))))))

(defun test-source-validation ()
  "Test in-memory validation, typed failures, and its independent bounds."
  (dolist (name (list "a" "release-notes" "skill-12" (make-string 64 :initial-element #\a)))
    (test-assert (eq (skill-name-valid-p name) t) "portable names are accepted"))
  (dolist (name (list nil 7 "" "Upper" "é" "a_b" "../a" "-a" "a-" "a--b"
                     (make-string 65 :initial-element #\a)))
    (test-assert (null (skill-name-valid-p name)) "invalid portable names are rejected"))
  (with-test-root (root)
    (let ((body (format nil "Exact λ body.~%  Keep indentation.~%")))
      (dolist (format '(:native :agent-skill))
        (let* ((pathname (merge-pathnames (if (eq format ':native)
                                             "alpha/SKILL.sexp"
                                             "alpha/SKILL.md") root))
               (source (if (eq format ':native)
                           (tests--native "alpha" "A   description." body)
                           (tests--standard "alpha" "A   description." body))))
          (multiple-value-bind (name description instructions)
              (skill-source-validate source pathname)
            (test-assert (string= name "alpha") "validation returns the source name")
            (test-assert (string= description "A description.") "description is normalized")
            (test-assert (string= instructions body) "instruction text is preserved"))))
      (test-assert (and (null (uiop:directory-files root))
                        (null (uiop:subdirectories root)))
                   "source validation creates no files or directories"))
    (let ((pathname (merge-pathnames "alpha/SKILL.md" root)))
      (test-assert (string= (third (multiple-value-list
                                   (skill-source-validate
                                    (tests--standard "alpha" "Empty body." "") pathname))) "")
                   "standard skills permit empty instructions")
      (dolist (case (list
                     (list "SKILL.md" "Missing frontmatter" ':invalid-syntax)
                     (list "alpha/SKILL.md" (tests--standard "other" "Mismatch." "Body") ':invalid-name)
                     (list "alpha/SKILL.md" (tests--standard "alpha--x" "Bad name." "Body") ':invalid-name)
                     (list "alpha/SKILL.md" (format nil "---~%name: alpha~%---~%Body") ':missing-field)
                     (list "alpha/SKILL.md" (format nil "---~%name: alpha~%name: alpha~%description: D~%---~%Body") ':duplicate-field)
                     (list "alpha/SKILL.sexp" (tests--native "alpha" "D" "") ':invalid-instructions)
                     (list "alpha/SKILL.sexp" "#.(error \"reader evaluation\")" ':invalid-syntax)
                     (list "alpha/skill.md" (tests--standard "alpha" "D" "Body") ':invalid-structure)))
        (let* ((target (merge-pathnames (first case) root))
               (condition (handler-case
                              (progn (skill-source-validate (second case) target) nil)
                            (skill-validation-error (condition) condition))))
          (test-assert (and condition
                            (eq (skill-validation-error-kind condition) (third case))
                            (equal (skill-validation-error-pathname condition) target))
                       "invalid source has a typed kind and intended pathname")))
      (let ((source (tests--standard "alpha" "D" "12345")))
        (dolist (options (list (list :file-character-limit (1- (length source)))
                              (list :instruction-character-limit 4)))
          (test-assert
           (handler-case
               (progn (apply #'skill-source-validate source pathname options) nil)
             (skill-validation-error (condition)
               (eq (skill-validation-error-kind condition) ':file-too-large)))
           "source and instruction bounds are enforced independently"))))))

(defun run-tests ()
  "Run the complete cl-skills test suite."
  (setf *test-count* 0)
  (test-path-classification)
  (test-source-validation)
  (test-discovery-and-precedence)
  (test-native-validation)
  (test-filesystem-boundaries)
  (test-canonical-discovery-roots)
  (test-literal-filesystem-entry-names)
  (test-symlinked-skill-directories)
  (test-scan-limits)
  (test-standard-validation)
  (test-conversion-cache)
  (test-catalog-rendering)
  (test-read-failures)
  (format t "~D cl-skills tests passed.~%" *test-count*)
  nil)
