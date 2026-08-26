(in-package #:cl-skills)

(-> skill--cache-identifier () string)
(defun skill--cache-identifier ()
  "Return a process-local identifier for one temporary cache file."
  (format nil "~D-~D-~D"
          (get-universal-time)
          (sb-posix:getpid)
          (random most-positive-fixnum)))

(-> skill--agent-native-source (string string string) string)
(defun skill--agent-native-source (name description instructions)
  "Return one generated native skill form preserving INSTRUCTIONS exactly."
  (let ((*print-pretty* nil)
        (*print-circle* nil)
        (*print-readably* t))
    (format nil
            "(:autolith-skill~% :version 1~% :name ~S~% :description ~S~% :instructions ~S)~%"
            name
            description
            instructions)))

(-> skill-standard-cache-pathname (pathname string) pathname)
(defun skill-standard-cache-pathname (cache-root digest)
  "Return DIGEST's generated native skill pathname below CACHE-ROOT."
  (merge-pathnames
   "SKILL.sexp"
   (merge-pathnames
    (format nil "skills/agent-skill-v1/~A/" digest)
    (uiop:ensure-directory-pathname cache-root))))

(-> skill--agent-cache-manifest-pathname (pathname) pathname)
(defun skill--agent-cache-manifest-pathname (cache-pathname)
  "Return CACHE-PATHNAME's sibling integrity manifest pathname."
  (merge-pathnames
   "manifest.sha256"
   (uiop:pathname-directory-pathname cache-pathname)))

(-> skill--agent-cache-manifest-source (string string) string)
(defun skill--agent-cache-manifest-source (source-digest cache-source)
  "Return the exact integrity manifest for SOURCE-DIGEST and CACHE-SOURCE."
  (format nil
          "source ~A~%cache ~A~%"
          source-digest
          (skill-source-digest cache-source)))

(-> skill--agent-cache-file-write (pathname string) pathname)
(defun skill--agent-cache-file-write (pathname content)
  "Atomically publish CONTENT at cache PATHNAME."
  (ensure-directories-exist pathname)
  (let* ((directory (uiop:pathname-directory-pathname pathname))
         (temporary
           (merge-pathnames
            (format nil ".cache.~A.tmp" (skill--cache-identifier))
            directory)))
    (unwind-protect
         (progn
           (with-open-file (stream temporary
                                   :direction ':output
                                   :if-exists ':error
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (write-string content stream)
             (finish-output stream))
           (sb-posix:chmod (sb-ext:native-namestring temporary) #o600)
           (uiop:rename-file-overwriting-target temporary pathname)
           (sb-posix:chmod (sb-ext:native-namestring pathname) #o600)
           pathname)
      (when (probe-file temporary)
        (ignore-errors (delete-file temporary))))))

(-> skill--agent-cache-write (pathname string string) pathname)
(defun skill--agent-cache-write (pathname content source-digest)
  "Publish native CONTENT and its SOURCE-DIGEST integrity manifest."
  (skill--agent-cache-file-write pathname content)
  (skill--agent-cache-file-write
   (skill--agent-cache-manifest-pathname pathname)
   (skill--agent-cache-manifest-source source-digest content))
  pathname)

(-> skill--agent-cache-read
    (pathname pathname string (integer 1))
    (values (option string) (option string) (option string) boolean))
(defun skill--agent-cache-read
    (pathname cache-root source-digest instruction-character-limit)
  "Return integrity-checked cached native values and true, or four NIL values."
  (handler-case
      (multiple-value-bind
            (cache-source canonical-pathname device inode)
          (skill--read-file-bounded
           pathname
           *skill-agent-cache-character-limit*
           :roots (list cache-root))
        (declare (ignore canonical-pathname device inode))
        (multiple-value-bind
              (manifest-source manifest-canonical manifest-device manifest-inode)
            (skill--read-file-bounded
             (skill--agent-cache-manifest-pathname pathname)
             256
             :roots (list cache-root))
          (declare (ignore manifest-canonical manifest-device manifest-inode))
          (unless (string=
                   manifest-source
                   (skill--agent-cache-manifest-source
                    source-digest
                    cache-source))
            (skill--definition-fail
             :invalid-structure
             "The SKILL.md conversion cache failed its integrity check."))
          (multiple-value-bind (name description instructions)
              (skill--parse-native-source
               cache-source
               :instruction-character-limit instruction-character-limit
               :allow-empty-instructions-p t)
            (values name description instructions t))))
    (skill--definition-error ()
      (values nil nil nil nil))))

(-> skill--parse-agent-definition
    (pathname
     &key (:instruction-character-limit (integer 1))
          (:file-character-limit (integer 1))
          (:roots list)
          (:cache-root (option pathname)))
    (values string string string pathname (integer 0)))
(defun skill--parse-agent-definition
    (pathname
     &key
       (instruction-character-limit *skill-instruction-character-limit*)
       (file-character-limit *skill-file-character-limit*)
       roots
       cache-root)
  "Read SKILL.md and use its content-addressed native conversion cache."
  (let ((*skill-definition-source-character-count* 0))
    (multiple-value-bind (source canonical-pathname device inode)
        (skill--read-file-bounded pathname file-character-limit :roots roots)
      (declare (ignore device inode))
      (let* ((source-character-count
               *skill-definition-source-character-count*)
             (digest (skill-source-digest source))
             (cache-pathname
               (and cache-root
                    (skill-standard-cache-pathname cache-root digest))))
        (multiple-value-bind
              (name description instructions cached-p)
            (if (and cache-pathname (probe-file cache-pathname))
                (skill--agent-cache-read
                 cache-pathname
                 cache-root
                 digest
                 instruction-character-limit)
                (values nil nil nil nil))
          (if cached-p
              (values (skill--validate-agent-name name pathname)
                      description
                      instructions
                      canonical-pathname
                      source-character-count)
              (multiple-value-bind (name description instructions)
                  (skill--parse-agent-source
                   source
                   pathname
                   :instruction-character-limit instruction-character-limit)
                (when cache-pathname
                  (handler-case
                      (skill--agent-cache-write
                       cache-pathname
                       (skill--agent-native-source
                        name
                        description
                        instructions)
                       digest)
                    (error (condition)
                      (skill--definition-fail
                       :read-error
                       "Could not publish SKILL.md conversion cache: ~A"
                       condition))))
                (values name
                        description
                        instructions
                        canonical-pathname
                        source-character-count))))))))

(-> skill--parse-source-definition
    (pathname
     &key (:instruction-character-limit (integer 1))
          (:file-character-limit (integer 1))
          (:roots list)
          (:cache-root (option pathname)))
    (values string string string pathname (integer 0)))
