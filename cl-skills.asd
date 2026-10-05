(asdf:defsystem #:cl-skills
  :description "Portable Agent Skills discovery, parsing, caching, and rendering."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:ironclad/digest/sha256
               #:ls-compat/posix
               #:nyaml
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

(asdf:defsystem #:cl-skills/executable
  :description "Optional executable Skill declarations and authorized invocation."
  :depends-on (#:cl-skills #:cl-llm-provider-api/contracts)
  :serial t
  :components ((:file "src/executable")
               (:file "src/executable-invocation"))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-skills/executable/tests))))

(asdf:defsystem #:cl-skills/executable/tests
  :description "Executable Skill discovery and real callable verification tests."
  :depends-on (#:cl-skills/tests #:cl-skills/executable)
  :components ((:file "tests/executable-tests"))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:cl-skills/tests '#:run-executable-tests)))
