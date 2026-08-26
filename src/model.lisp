(in-package #:cl-skills)

;;;; -- Policy --

(defparameter *skill-scan-depth-limit* 6
  "The maximum directory depth traversed below one skill root.")

(defparameter *skill-scan-directory-limit* 2000
  "The maximum directories inspected across all skill roots.")

(defparameter *skill-scan-entry-limit* 20000
  "The maximum filesystem entries inspected across all skill roots.")

(defparameter *skill-file-character-limit* (* 64 1024)
  "The maximum characters read from one skill source file.")

(defparameter *skill-agent-cache-character-limit*
  (+ (* 2 *skill-file-character-limit*) 4096)
  "The maximum characters read from one generated Agent Skill cache file.")

(defparameter *skill-discovery-character-limit* (* 8 1024 1024)
  "The maximum skill source characters read during one catalog discovery.")

(defparameter *skill-form-depth-limit* 32
  "The maximum structural depth accepted in one SKILL.sexp form.")

(defparameter *skill-form-node-limit* 128
  "The maximum conses and atoms accepted in one SKILL.sexp form.")

(defparameter *skill-instruction-character-limit* (* 64 1024)
  "The maximum instruction characters accepted from one selected skill.")

(defparameter *skill-description-character-limit* 1024
  "The maximum characters in a skill description.")

(defparameter *skill-catalog-character-budget* 3500
  "The default maximum characters rendered into a provider-visible catalog.")

(defparameter *skill-native-keywords*
  '(:autolith-skill :version :name :description :instructions)
  "The complete keyword vocabulary accepted by native skill forms.")

(defvar *skill-definition-source-character-count* 0
  "Characters read while validating the dynamically active skill definition.")


;;;; -- Value Model --

(deftype skill-source-format ()
  "The source representation backing one discovered skill."
  '(member :native :agent-skill))

(deftype skill-diagnostic-kind ()
  "A structured reason why skill discovery did not select one path."
  '(member :missing-root
           :scan-error
           :scan-depth-limit
           :scan-directory-limit
           :scan-entry-limit
           :scan-character-limit
           :outside-root
           :not-regular-file
           :identity-changed
           :read-error
           :file-too-large
           :data-too-deep
           :data-too-large
           :invalid-syntax
           :invalid-structure
           :missing-field
           :unknown-field
           :duplicate-field
           :invalid-version
           :invalid-name
           :invalid-description
           :invalid-instructions
           :shadowed))

(defclass skill-metadata ()
  ((name
    :initarg :name
    :reader skill-metadata-name
    :type non-empty-string
    :documentation "The validated skill name from the source definition.")
   (description
    :initarg :description
    :reader skill-metadata-description
    :type non-empty-string
    :documentation "The bounded single-line description used for selection.")
   (pathname
    :initarg :pathname
    :reader skill-metadata-pathname
    :type pathname
    :documentation "The exact absolute discovered skill source pathname.")
   (canonical-pathname
    :initarg :canonical-pathname
    :reader skill-metadata-canonical-pathname
    :type pathname
    :documentation "The canonical regular file read during discovery.")
   (root
    :initarg :root
    :reader skill-metadata-root
    :type pathname
    :documentation "The ordered discovery root that supplied this skill.")
    (confinement-roots
     :initarg :confinement-roots
     :reader skill-metadata--confinement-roots
     :type list
     :documentation "The configured roots that confine discovery and fresh reads.")
   (root-index
    :initarg :root-index
    :reader skill-metadata-root-index
    :type (integer 0)
    :documentation "The zero-based precedence position of the discovery root.")
   (source-format
    :initarg :source-format
    :reader skill-metadata-source-format
    :type skill-source-format
    :documentation "The native or standard source representation.")
   (cache-root
    :initarg :cache-root
    :initform nil
    :reader skill-metadata-cache-root
    :type (or null pathname)
    :documentation "The replaceable conversion-cache root for standard Skills."))
  (:documentation
   "Validated skill catalog metadata without retained instruction text."))

(defclass skill-diagnostic ()
  ((kind
    :initarg :kind
    :reader skill-diagnostic-kind
    :type skill-diagnostic-kind
    :documentation "The machine-readable discovery or validation outcome.")
   (pathname
    :initarg :pathname
    :reader skill-diagnostic-pathname
    :type pathname
    :documentation "The file or directory associated with the outcome.")
   (root-index
    :initarg :root-index
    :reader skill-diagnostic-root-index
    :type (integer 0)
    :documentation "The zero-based discovery-root position.")
   (message
    :initarg :message
    :reader skill-diagnostic-message
    :type non-empty-string
    :documentation "A concise human-readable explanation."))
  (:documentation
   "One typed skill discovery result that does not abort the remaining scan."))

(defclass skill-catalog ()
  ((skills
    :initarg :skills
    :reader skill-catalog-skills
    :type list
    :documentation "Selected metadata in deterministic precedence order.")
   (diagnostics
    :initarg :diagnostics
    :reader skill-catalog-diagnostics
    :type list
    :documentation "Non-fatal scan, parse, validation, and shadowing outcomes."))
  (:documentation
   "An immutable skill metadata snapshot assembled from ordered roots."))

(define-condition skill-error (error)
  ((message
    :initarg :message
    :reader skill-error-message
    :type string
    :documentation "A human-readable explanation."))
  (:documentation "The base condition for cl-skills failures.")
  (:report (lambda (condition stream)
             (write-string (skill-error-message condition) stream))))

(define-condition skill-read-error (skill-error)
  ((pathname
    :initarg :pathname
    :reader skill-read-error-pathname
    :type pathname
    :documentation "The selected skill source that could not be read.")
   (cause
    :initarg :cause
    :initform nil
    :reader skill-read-error-cause
    :type t
    :documentation "The underlying filesystem, syntax, or validation failure."))
  (:documentation "Reading selected skill instructions failed."))

(define-condition skill-body-too-large (skill-read-error)
  ((character-limit
    :initarg :character-limit
    :reader skill-body-too-large-character-limit
    :type (integer 1)
    :documentation "The maximum selected instruction size in characters."))
  (:documentation "Selected skill instructions exceed the input bound."))

(define-condition skill-catalog-render-error (skill-error)
  ((character-budget
    :initarg :character-budget
    :reader skill-catalog-render-error-character-budget
    :type (integer 1)
    :documentation "The requested maximum rendered character count.")
   (minimum-required
    :initarg :minimum-required
    :reader skill-catalog-render-error-minimum-required
    :type (integer 1)
    :documentation "The characters needed for the catalog protocol itself."))
  (:documentation "A catalog budget cannot hold its required guidance."))

(define-condition skill--definition-error (error)
  ((kind
    :initarg :kind
    :reader skill--definition-error-kind
    :type skill-diagnostic-kind
    :documentation "The diagnostic kind produced for this file.")
   (message
    :initarg :message
    :reader skill--definition-error-message
    :type non-empty-string
    :documentation "The validation failure explanation.")
   (source-character-count
    :initarg :source-character-count
    :initform 0
    :reader skill--definition-error-source-character-count
    :type (integer 0)
    :documentation "Characters read before this definition failed."))
  (:documentation "An internal non-fatal definition validation failure.")
  (:report (lambda (condition stream)
             (write-string (skill--definition-error-message condition)
                           stream))))
