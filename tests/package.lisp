(defpackage #:cl-skills/tests
  (:documentation "Tests for cl-skills discovery, parsing, caching, rendering, and executable Skills.")
  (:use #:cl)
  (:import-from #:cl-skills
                #:*skill-description-character-limit*
                #:*skill-file-character-limit*
                #:*skill-form-depth-limit*
                #:*skill-form-node-limit*
                #:*skill-instruction-character-limit*
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
                #:skill-diagnostic-kind
                #:skill-metadata-cache-root
                #:skill-metadata-canonical-pathname
                #:skill-metadata-description
                #:skill-metadata-discover
                #:skill-metadata-name
                #:skill-metadata-read
                #:skill-metadata-root
                #:skill-metadata-root-index
                #:skill-metadata-source-format
                #:skill-name-valid-p
                #:skill-read-error
                #:skill-read-error-cause
                #:skill-read-error-pathname
                #:skill-source-digest
                #:skill-source-format-for-pathname
                #:skill-source-pathname-p
                #:skill-source-validate
                #:skill-standard-cache-pathname
                #:skill-validation-error
                #:skill-validation-error-kind
                #:skill-validation-error-pathname)
  (:import-from #:serapeum
                #:partial))
