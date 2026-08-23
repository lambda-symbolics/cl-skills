(asdf:defsystem #:cl-skills
  :description "Portable Agent Skills discovery, parsing, caching, and rendering."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:ironclad/digest/sha256
               #:nyaml
               #:sb-posix
               #:sexp-config
               #:serapeum)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "model")
                             (:file "discovery")
                             (:file "parsing")
                             (:file "cache")
                             (:file "catalog")
                             (:file "render"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-skills/tests))))

(asdf:defsystem #:cl-skills/tests
  :description "Tests for cl-skills."
  :depends-on (#:cl-skills)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:cl-skills/tests '#:run-tests)))
