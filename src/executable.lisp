(in-package #:cl-skills)

;;;; -- Executable Skill Metadata --

(export '(*skill-executable-character-limit* *skill-executable-node-limit*
          *skill-executable-depth-limit* skill-executable skill-executable-discover
          skill-executable-pathname skill-executable-digest skill-executable-manifest
          skill-executable-error skill-executable-error-kind
          skill-executable-error-pathname skill-executable-error-cause
          skill-executable-invoke skill-executable-verify))

(defparameter *skill-executable-character-limit* 32768
  "Maximum characters read from one EXECUTABLE.sexp sidecar.")

(defparameter *skill-executable-node-limit* 4096
  "Maximum nodes in a sidecar or portable invocation value.")

(defparameter *skill-executable-depth-limit* 48
  "Maximum depth of a sidecar or portable invocation value.")

(defparameter *skill-executable-keywords*
  '(:skill-executable :format-version :skill-version :system :system-version
    :entrypoint :self-test :verify :dependencies :capabilities :tools
    :input :output :type :enum :object :array :string :integer :number
    :boolean :null :properties :required :additional-properties :items
    :min-items :max-items)
  "Data-only vocabulary for executable sidecars and native contracts.")

(define-condition skill-executable-error (skill-error)
  ((kind :initarg :kind :reader skill-executable-error-kind :type keyword
         :documentation "Machine-readable failure category.")
   (pathname :initarg :pathname :reader skill-executable-error-pathname :type pathname
             :documentation "Sidecar pathname associated with the failure.")
   (cause :initarg :cause :initform nil :reader skill-executable-error-cause
          :type (or null condition)
          :documentation "Underlying condition, when available."))
  (:documentation "An executable skill discovery, authorization or invocation failure."))

(defclass skill-executable ()
  ((pathname :initarg :pathname :reader skill-executable-pathname :type pathname
             :documentation "Discovered absolute sidecar pathname.")
   (canonical-pathname :initarg :canonical-pathname :type pathname
                       :reader skill-executable--canonical-pathname
                       :documentation "Canonical file selected by discovery.")
   (identity :initarg :identity :reader skill-executable--identity
             :documentation "Filesystem identity selected by discovery.")
   (digest :initarg :digest :reader skill-executable--digest :type string
           :documentation "SHA-256 of exact UTF-8 sidecar content.")
   (manifest :initarg :manifest :reader skill-executable--manifest :type list
             :documentation "Validated data-only declaration.")
   (skill :initarg :skill :reader skill-executable--skill :type skill-metadata
          :documentation "Original selected skill metadata."))
  (:documentation "A discovered executable declaration, without loaded code or authority."))

(-> skill-executable-digest (skill-executable) string)
(defun skill-executable-digest (executable)
  "Return a fresh copy of EXECUTABLE's content identity."
  (copy-seq (skill-executable--digest executable)))

(defun skill-executable--fail (pathname kind message &optional cause)
  "Signal a typed executable failure without including invocation payloads."
  (error 'skill-executable-error :pathname pathname :kind kind
         :message message :cause cause))

(-> skill-executable-manifest (skill-executable) list)
(defun skill-executable-manifest (executable)
  "Return a fresh portable copy of EXECUTABLE's validated declaration."
  (labels ((copy (value)
             (typecase value
               (cons (cons (copy (first value)) (copy (rest value))))
               (string (copy-seq value))
               (t value))))
    (copy (skill-executable--manifest executable))))

(defun skill-executable--string-p (value)
  "Return true for bounded non-empty metadata strings without control characters."
  (and (stringp value) (<= 1 (length value) 256)
       (every (lambda (character) (>= (char-code character) 32)) value)))

(defun skill-executable--names-p (values)
  "Return true for a bounded duplicate-free list of metadata names."
  (and (listp values) (<= (length values) 128)
       (every #'skill-executable--string-p values)
       (= (length values) (length (remove-duplicates values :test #'string=)))))

(defun skill-executable--entrypoint-p (value)
  "Return true for a (package-name symbol-name) declaration, without interning."
  (and (listp value) (= (length value) 2)
       (every #'skill-executable--string-p value)))

(defun skill-executable--version-p (value)
  "Return true for the dotted decimal version syntax supported by ASDF."
  (and (skill-executable--string-p value)
       (digit-char-p (char value 0))
       (digit-char-p (char value (1- (length value))))
       (loop for character across value for index from 0
             always (or (not (null (digit-char-p character)))
                        (and (char= character #\.) (plusp index)
                             (not (null (digit-char-p (char value (1- index))))))))))

(defun skill-executable--validate (form pathname)
  "Validate a safely-read sidecar and reuse provider contract normalization."
  (unless (and (consp form) (eq (first form) ':skill-executable)
               (evenp (length (rest form))))
    (skill-executable--fail pathname ':invalid-structure
                            "Expected one :SKILL-EXECUTABLE property list."))
  (let ((manifest (rest form)) (seen nil)
        (allowed '(:format-version :skill-version :system :system-version
                   :entrypoint :self-test :verify :dependencies :capabilities
                   :tools :input :output)))
    (loop for tail on manifest by #'cddr for key = (first tail) do
      (unless (member key allowed)
        (skill-executable--fail pathname ':unknown-field "Unknown executable field."))
      (when (member key seen)
        (skill-executable--fail pathname ':duplicate-field "Duplicate executable field."))
      (push key seen))
    (unless (eql (getf manifest :format-version) 1)
      (skill-executable--fail pathname ':invalid-version "Unsupported executable format version."))
    (unless (skill-executable--string-p (getf manifest :skill-version))
      (skill-executable--fail pathname ':invalid-version "A bounded skill version string is required."))
    (unless (or (getf manifest :system) (getf manifest :entrypoint))
      (skill-executable--fail pathname ':missing-field "Declare a system or an entrypoint."))
    (dolist (key '(:system :system-version))
      (when (member key seen)
        (unless (skill-executable--string-p (getf manifest key))
          (skill-executable--fail pathname ':invalid-structure "Invalid system or version name."))))
    (when (and (getf manifest :system-version) (not (getf manifest :system)))
      (skill-executable--fail pathname ':invalid-structure "A system version requires a system."))
    (when (and (getf manifest :system-version)
               (not (skill-executable--version-p (getf manifest :system-version))))
      (skill-executable--fail pathname ':invalid-version "Invalid ASDF system version syntax."))
    (dolist (key '(:entrypoint :self-test :verify))
      (when (member key seen)
        (unless (skill-executable--entrypoint-p (getf manifest key))
          (skill-executable--fail pathname ':invalid-entrypoint "Entrypoints require package and symbol strings."))))
    (dolist (key '(:capabilities :tools))
      (unless (skill-executable--names-p (getf manifest key))
        (skill-executable--fail pathname ':invalid-structure "Invalid capability or tool names.")))
    (let ((dependencies (getf manifest :dependencies)))
      (unless (and (listp dependencies) (<= (length dependencies) 128)
                   (every #'skill-executable--entrypoint-p dependencies)
                   (= (length dependencies)
                      (length (remove-duplicates dependencies :key #'first :test #'string=))))
        (skill-executable--fail pathname ':invalid-structure
                                "Dependencies require unique (system minimum-version) pairs."))
      (unless (every (lambda (dependency) (skill-executable--version-p (second dependency))) dependencies)
        (skill-executable--fail pathname ':invalid-version "Invalid dependency version syntax.")))
    (when (getf manifest :entrypoint)
      (unless (and (member :input seen) (member :output seen))
        (skill-executable--fail pathname ':missing-field
                                "Callable skills require input and output contracts.")))
    (dolist (key '(:input :output))
      (when (member key seen)
        (setf (getf manifest key)
              (handler-case
                  (cl-llm-provider-api:output-schema-normalize
                   (getf manifest key) :maximum-nodes *skill-executable-node-limit*
                   :maximum-depth *skill-executable-depth-limit* :property-name-limit 256)
                (error (condition)
                  (skill-executable--fail pathname ':invalid-contract
                                          "Invalid executable contract." condition))))))
    manifest))

(defun skill-executable--read (pathname roots)
  "Read a confined regular sidecar with the existing stable-identity reader."
  (handler-case
      (skill--read-file-bounded pathname *skill-executable-character-limit* :roots roots)
    (skill--definition-error (condition)
      (skill-executable--fail pathname (skill--definition-error-kind condition)
                              "Could not read executable sidecar." condition))))

(-> skill-executable-discover (skill-metadata) (option skill-executable))
(defun skill-executable-discover (skill)
  "Return executable metadata or NIL for a missing optional sidecar.

Read only EXECUTABLE.sexp beside the original SKILL.md or SKILL.sexp. Never
load an ASDF definition, resolve an entrypoint, or grant declared authority."
  (let* ((source (skill-metadata-pathname skill))
         (directory (uiop:pathname-directory-pathname source))
         (pathname (merge-pathnames "EXECUTABLE.sexp" directory))
         (roots (skill-metadata--confinement-roots skill)))
    (handler-case
        (unless (equal (ls-compat.posix:canonical-pathname source)
                       (skill-metadata-canonical-pathname skill))
          (skill-executable--fail pathname ':identity-changed "The selected skill source moved."))
      (skill-executable-error (condition) (error condition))
      (error (condition)
        (skill-executable--fail pathname ':identity-changed "The selected skill source is unavailable." condition)))
    ;; LSTAT distinguishes a missing sidecar from a dangling link or other
    ;; non-regular entry. The confined reader handles every existing candidate.
    (handler-case (ls-compat.posix:file-information pathname :follow-links-p nil)
      (ls-compat.posix:file-operation-failed (condition)
        (if (eq (ls-compat.posix:file-operation-failed-reason condition) ':missing)
            (return-from skill-executable-discover nil)
            (skill-executable--fail pathname ':read-error "Could not inspect executable sidecar." condition))))
    (multiple-value-bind (text canonical identity) (skill-executable--read pathname roots)
      (let* ((grammar (make-source-grammar
                       :label "EXECUTABLE.sexp" :keywords *skill-executable-keywords*
                       :maximum-depth *skill-executable-depth-limit*
                       :maximum-nodes *skill-executable-node-limit*
                       :common-lisp-symbols-permitted-p t
                       :allowed-atom-predicate
                       (lambda (value)
                         (or (null value) (eq value t) (stringp value)
                             (numberp value) (member value *skill-executable-keywords*)))))
             (form (handler-case (read-source text grammar)
                     (sexp-config-error (condition)
                       (skill-executable--fail pathname (sexp-config-error-kind condition)
                                               "Invalid executable sidecar data." condition)))))
        (make-instance 'skill-executable :pathname pathname :canonical-pathname canonical
                       :identity identity :digest (skill-source-digest text)
                       :manifest (skill-executable--validate form pathname) :skill skill)))))

(defun skill-executable--fresh-p (executable)
  "Reject path, filesystem identity or content changes after discovery."
  (let* ((pathname (skill-executable-pathname executable))
         (skill (skill-executable--skill executable)))
    (handler-case
        (unless (equal (ls-compat.posix:canonical-pathname (skill-metadata-pathname skill))
                       (skill-metadata-canonical-pathname skill))
          (skill-executable--fail pathname ':identity-changed "The selected skill source moved."))
      (skill-executable-error (condition) (error condition))
      (error (condition)
        (skill-executable--fail pathname ':identity-changed "The selected skill source is unavailable." condition)))
    (multiple-value-bind (text canonical identity)
        (skill-executable--read pathname (skill-metadata--confinement-roots skill))
      (unless (and (equal canonical (skill-executable--canonical-pathname executable))
                   (equal identity (skill-executable--identity executable))
                   (string= (skill-source-digest text) (skill-executable-digest executable)))
        (skill-executable--fail pathname ':identity-changed "The executable declaration changed after discovery.")))
    t))
