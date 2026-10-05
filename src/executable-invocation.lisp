(in-package #:cl-skills)

;;;; -- Authorized Invocation --

(defun skill-executable--cancel (executable cancelled-p)
  "Signal cancellation or predicate failure at a cooperative invocation boundary."
  (when (and cancelled-p
             (handler-case (funcall cancelled-p)
               (error (condition)
                 (skill-executable--fail (skill-executable-pathname executable)
                                         ':cancellation-error
                                         "The cancellation predicate failed." condition))))
    (skill-executable--fail (skill-executable-pathname executable) ':cancelled
                            "Executable invocation was cancelled.")))

(defun skill-executable--bounded-value (executable value kind)
  "Bound portable value traversal before delegating contract conversion.

Count cons cells, atoms and string characters. Reject cycles without traversing
an unbounded list. This is a resource bound, not a second contract validator."
  (let ((nodes 0) (characters 0) (active (make-hash-table :test #'eq))
        (pathname (skill-executable-pathname executable)))
    (labels ((visit (value depth)
               (when (or (> (incf nodes) *skill-executable-node-limit*)
                         (> depth *skill-executable-depth-limit*))
                 (skill-executable--fail pathname kind "Invocation value exceeds structural bounds."))
               (typecase value
                 (cons
                  (when (gethash value active)
                    (skill-executable--fail pathname kind "Invocation value contains a cycle."))
                  (setf (gethash value active) t)
                  (visit (first value) (1+ depth))
                  (visit (rest value) depth)
                  (remhash value active))
                 (string
                  (when (> (incf characters (length value)) *skill-executable-character-limit*)
                    (skill-executable--fail pathname kind "Invocation value exceeds character bounds."))))))
      (visit value 0))))

(defun skill-executable--contract-value (executable value key)
  "Return fresh normalized tagged VALUE satisfying the declared KEY contract."
  (let ((kind (if (eq key ':input) ':invalid-input ':invalid-output))
        (pathname (skill-executable-pathname executable)))
    (skill-executable--bounded-value executable value kind)
    (handler-case
        (let ((json (cl-llm-provider-api:output-sexp->json value)))
          (unless (cl-llm-provider-api:output-schema-valid-p
                   json (getf (skill-executable--manifest executable) key))
            (skill-executable--fail pathname kind "Invocation value does not satisfy its contract."))
          (cl-llm-provider-api:output-json->sexp json))
      (skill-executable-error (condition) (error condition))
      (error (condition)
        (skill-executable--fail pathname kind "Malformed portable invocation value." condition)))))

(defun skill-executable--resolve (executable declaration)
  "Resolve an existing function without reading or interning the declaration."
  (let* ((package (find-package (first declaration)))
         (symbol (and package (find-symbol (second declaration) package))))
    (unless (and symbol (fboundp symbol) (not (macro-function symbol))
                 (not (special-operator-p symbol)))
      (skill-executable--fail (skill-executable-pathname executable) ':missing-entrypoint
                              "The declared callable entrypoint is unavailable."))
    (symbol-function symbol)))

(defun skill-executable--check-version (executable name required exact-p)
  "Check an authorized ASDF system definition before loading its code."
  (let ((pathname (skill-executable-pathname executable)))
    (handler-case
        (let* ((system (asdf:find-system name nil))
               (version (and system (asdf:component-version system))))
          (unless system
            (skill-executable--fail pathname ':missing-system "A declared ASDF system is unavailable."))
          (when (and required
                     (not (and version
                               (if exact-p (string= version required)
                                   (asdf:version-satisfies version required)))))
            (skill-executable--fail pathname ':version-mismatch "An ASDF system version does not satisfy the declaration.")))
      (skill-executable-error (condition) (error condition))
      (error (condition)
        (skill-executable--fail pathname ':system-error "Could not resolve a declared ASDF system." condition)))))

(defun skill-executable--prepare (executable &key authorize loader context operation cancelled-p)
  "Authorize, revalidate identity, check versions, and explicitly load declared systems."
  (let* ((manifest (skill-executable--manifest executable))
         (pathname (skill-executable-pathname executable))
         (system (getf manifest :system))
         (dependencies (getf manifest :dependencies)))
    (skill-executable--cancel executable cancelled-p)
    (skill-executable--fresh-p executable)
    (unless (and authorize
                 (handler-case
                     (eq t (funcall authorize executable :operation operation :context context))
                   (error (condition)
                     (skill-executable--fail pathname ':authorization-error
                                             "Executable authorization failed." condition))))
      (skill-executable--fail pathname ':not-authorized "Executable invocation requires explicit authorization."))
    (skill-executable--cancel executable cancelled-p)
    (skill-executable--fresh-p executable)
    (when (and (or system dependencies) (not loader))
      (skill-executable--fail pathname ':loader-required "Declared systems require an explicit loader callback."))
    ;; ASDF definition lookup can execute an ASD. Perform it only after the
    ;; caller has authorized this invocation, never during discovery.
    (dolist (dependency dependencies)
      (skill-executable--check-version executable (first dependency) (second dependency) nil))
    (when system
      (skill-executable--check-version executable system (getf manifest :system-version) t))
    (dolist (name (remove-duplicates (append (mapcar #'first dependencies) (when system (list system)))
                                   :test #'string=))
      (skill-executable--cancel executable cancelled-p)
      (handler-case (funcall loader name :context context)
        (error (condition)
          (skill-executable--fail pathname ':load-error "The executable system loader failed." condition))))
    ;; Loading may replace system definitions. Check the effective versions.
    (dolist (dependency dependencies)
      (skill-executable--check-version executable (first dependency) (second dependency) nil))
    (when system
      (skill-executable--check-version executable system (getf manifest :system-version) t))
    (skill-executable--cancel executable cancelled-p)
    (skill-executable--fresh-p executable)))

(-> skill-executable-invoke
    (skill-executable t &key (:authorize t) (:loader t) (:context t) (:cancelled-p t)) t)
(defun skill-executable-invoke (executable input &key authorize loader context cancelled-p)
  "Invoke a declared function as (FUNCTION INPUT :CONTEXT CONTEXT).

INPUT and the primary result use provider API portable tagged values. AUTHORIZE
must return T for (EXECUTABLE :OPERATION :INVOKE :CONTEXT CONTEXT), including all
declared capabilities and tools. LOADER, required for declared systems, receives
(SYSTEM-NAME :CONTEXT CONTEXT). No authority is inferred from metadata. The caller
owns process supervision, timeouts and actual capability enforcement."
  (let* ((pathname (skill-executable-pathname executable))
         (declaration (getf (skill-executable--manifest executable) :entrypoint)))
    (unless declaration
      (skill-executable--fail pathname ':missing-entrypoint "The skill declares no invocation entrypoint."))
    ;; Reject bad input before authorization or loading.
    (let ((input (skill-executable--contract-value executable input ':input)))
      (skill-executable--prepare executable :authorize authorize :loader loader
                                :context context :operation ':invoke :cancelled-p cancelled-p)
      (let* ((function (skill-executable--resolve executable declaration))
             (output (handler-case (funcall function input :context context)
                       (error (condition)
                         (skill-executable--fail pathname ':invocation-error
                                                 "The executable entrypoint failed." condition)))))
        (skill-executable--cancel executable cancelled-p)
        (skill-executable--fresh-p executable)
        (skill-executable--contract-value executable output ':output)))))

(-> skill-executable-verify
    (skill-executable &key (:entrypoint t) (:authorize t) (:loader t)
     (:context t) (:cancelled-p t)) boolean)
(defun skill-executable-verify (executable &key (entrypoint ':self-test) authorize loader context cancelled-p)
  "Call the declared :SELF-TEST or :VERIFY function as (FUNCTION :CONTEXT CONTEXT).

Return T only when its primary result is exactly T. A missing declaration,
non-T result, error, cancellation or changed declaration is a typed failure.
Verification is caller-requested execution, not a discovery-time efficacy claim."
  (let* ((pathname (skill-executable-pathname executable))
         (declaration (and (member entrypoint '(:self-test :verify))
                           (getf (skill-executable--manifest executable) entrypoint))))
    (unless declaration
      (skill-executable--fail pathname ':missing-entrypoint "The requested verification entrypoint is not declared."))
    (skill-executable--prepare executable :authorize authorize :loader loader
                              :context context :operation entrypoint :cancelled-p cancelled-p)
    (let ((result (handler-case
                      (funcall (skill-executable--resolve executable declaration) :context context)
                    (skill-executable-error (condition) (error condition))
                    (error (condition)
                      (skill-executable--fail pathname ':verification-error
                                              "The verification entrypoint failed." condition)))))
      (skill-executable--cancel executable cancelled-p)
      (skill-executable--fresh-p executable)
      (unless (eq result t)
        (skill-executable--fail pathname ':verification-failed "The verification entrypoint did not return T."))
      t)))
