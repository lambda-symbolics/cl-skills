(defpackage #:cl-skills
  (:use #:cl)
  (:import-from #:ironclad
                #:digest-sequence)
  (:import-from #:serapeum
                #:->)
  (:export
   #:*skill-agent-cache-character-limit*
   #:*skill-catalog-character-budget*
   #:*skill-description-character-limit*
   #:*skill-discovery-character-limit*
   #:*skill-file-character-limit*
   #:*skill-form-depth-limit*
   #:*skill-form-node-limit*
   #:*skill-instruction-character-limit*
   #:*skill-native-keywords*
   #:*skill-scan-depth-limit*
   #:*skill-scan-directory-limit*
   #:*skill-scan-entry-limit*
   #:skill-body-too-large
   #:skill-body-too-large-character-limit
   #:skill-catalog
   #:skill-catalog-diagnostics
   #:skill-catalog-discover
   #:skill-catalog-find
   #:skill-catalog-render
   #:skill-catalog-render-error
   #:skill-catalog-render-error-character-budget
   #:skill-catalog-render-error-minimum-required
   #:skill-catalog-skills
   #:skill-diagnostic
   #:skill-diagnostic-kind
   #:skill-diagnostic-message
   #:skill-diagnostic-pathname
   #:skill-diagnostic-root-index
   #:skill-metadata
   #:skill-metadata-cache-root
   #:skill-metadata-canonical-pathname
   #:skill-metadata-description
   #:skill-metadata-name
   #:skill-metadata-pathname
   #:skill-metadata-read
   #:skill-metadata-root
   #:skill-metadata-root-index
   #:skill-metadata-source-format
   #:skill-read-error
   #:skill-read-error-cause
   #:skill-read-error-pathname
   #:skill-source-format
   #:skill-source-format-for-pathname
   #:skill-source-pathname-p))

(in-package #:cl-skills)

(deftype option (inner-type)
  "A value that is either NIL or an instance of INNER-TYPE."
  `(or null ,inner-type))

(deftype non-empty-string ()
  "A string containing at least one character."
  '(and string (satisfies string-not-empty-p)))

(defun string-not-empty-p (value)
  "Return true when VALUE is a non-empty string."
  (and (stringp value)
       (plusp (length value))))
