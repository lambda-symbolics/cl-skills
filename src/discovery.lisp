(in-package #:cl-skills)

;;;; -- Filesystem Discovery --

(-> skill--diagnostic
    (&key (:kind skill-diagnostic-kind)
          (:pathname pathname)
          (:root-index (integer 0))
          (:message string))
    skill-diagnostic)
(defun skill--diagnostic
    (&key kind pathname root-index message)
  "Return one structured diagnostic for PATHNAME in ROOT-INDEX."
  (make-instance 'skill-diagnostic
                 :kind kind
                 :pathname pathname
                 :root-index root-index
                 :message message))

(-> skill--pathname< (pathname pathname) boolean)
(defun skill--pathname< (left right)
  "Return true when LEFT sorts before RIGHT by its namestring."
  (not (null (string< (namestring left) (namestring right)))))

(-> skill-source-format-for-pathname (pathname) (option skill-source-format))
(defun skill-source-format-for-pathname (pathname)
  "Return PATHNAME's exact supported skill source format, if any."
  (cond
    ((string= (file-namestring pathname) "SKILL.sexp")
     ':native)
    ((string= (file-namestring pathname) "SKILL.md")
     ':agent-skill)))

(-> skill-source-pathname-p (pathname) boolean)
(defun skill-source-pathname-p (pathname)
  "Return true when PATHNAME names a supported case-sensitive skill file."
  (not (null (skill-source-format-for-pathname pathname))))

(-> skill--definition-pathname< (pathname pathname) boolean)
(defun skill--definition-pathname< (left right)
  "Sort definitions by path while preferring native files in one directory."
  (let ((left-directory
          (namestring (uiop:pathname-directory-pathname left)))
        (right-directory
          (namestring (uiop:pathname-directory-pathname right))))
    (if (string= left-directory right-directory)
        (and (eq (skill-source-format-for-pathname left) ':native)
             (eq (skill-source-format-for-pathname right) ':agent-skill))
        (not (null (string< (namestring left) (namestring right)))))))

(-> skill--canonical-subpath-p (pathname pathname) boolean)
(defun skill--canonical-subpath-p (pathname root)
  "Return true when canonical PATHNAME is ROOT or lies beneath it."
  (or (uiop:pathname-equal pathname root)
      (not (null (uiop:subpathp pathname root)))))

(-> skill--directory-entry-pathname
    (pathname string &key (:directory-p boolean))
    pathname)
(defun skill--directory-entry-pathname (directory name &key directory-p)
  "Return NAME beneath DIRECTORY using the host's literal filename syntax."
  (sb-ext:parse-native-namestring
   (concatenate 'string (sb-ext:native-namestring directory) name)
   nil
   *default-pathname-defaults*
   :as-directory directory-p))

(-> skill--canonical-roots (list) list)
(defun skill--canonical-roots (roots)
  "Return the canonical existing directory pathnames among ROOTS."
  (loop for root in roots
        for canonical = (handler-case
                            (truename root)
                          (error ()
                            nil))
        when canonical
          collect (uiop:ensure-directory-pathname canonical)))

(-> skill--canonical-pathname-confined-p (pathname list) boolean)
(defun skill--canonical-pathname-confined-p (pathname canonical-roots)
  "Return true when canonical PATHNAME lies within CANONICAL-ROOTS."
  (not
   (null
    (find-if (lambda (root)
               (skill--canonical-subpath-p pathname root))
             canonical-roots))))

(-> skill--directory-entries-bounded
    (pathname (integer 0))
    (values list list boolean (integer 0) list))
(defun skill--directory-entries-bounded (directory entry-limit)
  "Return bounded files and subdirectories directly beneath DIRECTORY.

The third value is true when more than ENTRY-LIMIT entries exist. The fourth
value is the number of entries retained toward the aggregate scan budget. The
fifth value contains unresolved symbolic links.
Enumeration stops after the first excess entry and never retains an unbounded
directory listing."
  (let ((files nil)
        (subdirectories nil)
        (unresolved-links nil))
    (multiple-value-bind (entries exceeded-p)
        (ls-compat.posix:directory-entries directory :limit entry-limit)
      (if exceeded-p
          (values nil nil t entry-limit nil)
          (progn
            (dolist (entry entries)
              (let ((name (car entry))
                    (kind (cdr entry)))
                (let ((pathname (skill--directory-entry-pathname directory name)))
                  (case kind
                    (:directory
                     (push (skill--directory-entry-pathname
                            directory name :directory-p t)
                           subdirectories))
                    (:symbolic-link
                     (handler-case
                         (if (eq (ls-compat.posix:file-information-kind
                                  (ls-compat.posix:file-information
                                   pathname :follow-links-p t))
                                 ':directory)
                             (push (skill--directory-entry-pathname
                                    directory name :directory-p t)
                                   subdirectories)
                             (when (skill-source-pathname-p pathname)
                               (push pathname files)))
                       (error ()
                         (push pathname unresolved-links))))
                    (t
                     (when (or (eq kind ':file)
                               (skill-source-pathname-p pathname))
                       (push pathname files)))))))
            (values (sort files #'skill--pathname<)
                    (sort subdirectories #'skill--pathname<)
                    nil
                    (length entries)
                    (sort unresolved-links #'skill--pathname<)))))))

(-> skill--scan-root
    (pathname (integer 0)
     &key (:max-depth (integer 0))
          (:max-directories (integer 1))
          (:max-entries (integer 1))
          (:confinement-roots list))
    (values list list (integer 0) (integer 0)))
(defun skill--scan-root
    (root root-index
     &key
       (max-depth *skill-scan-depth-limit*)
       (max-directories *skill-scan-directory-limit*)
       (max-entries *skill-scan-entry-limit*)
       confinement-roots)
  "Return sorted skill definition paths and diagnostics found beneath ROOT."
  (let ((root (uiop:ensure-directory-pathname root))
        (canonical-root nil)
        (canonical-confinement-roots nil)
        (paths nil)
        (diagnostics nil)
        (visited (make-hash-table :test #'equal))
        (directory-count 0)
        (entry-count 0)
        (stopped-p nil))
    (labels
        ((record-diagnostic (kind pathname message)
           (push (skill--diagnostic
                  :kind kind
                  :pathname pathname
                  :root-index root-index
                  :message message)
                 diagnostics))

         (stop-scan (kind pathname message)
           (unless stopped-p
             (setf stopped-p t)
             (record-diagnostic kind pathname message)))

         (walk (directory depth)
           (block nil
             (when stopped-p
               (return))
             (when (>= entry-count max-entries)
               (stop-scan :scan-entry-limit
                          directory
                          (format nil
                                  "Skill scan reached its ~D entry limit."
                                  max-entries))
               (return))
             (when (>= directory-count max-directories)
               (stop-scan :scan-directory-limit
                          directory
                          (format nil
                                  "Skill scan reached its ~D directory limit."
                                  max-directories))
               (return))
             (incf directory-count)
             (let ((canonical
                     (handler-case
                         (truename directory)
                       (error (condition)
                         (record-diagnostic
                          :scan-error
                          directory
                          (format nil
                                  "Could not resolve skill directory: ~A"
                                  condition))
                         nil))))
               (unless canonical
                 (return))
                (unless (skill--canonical-pathname-confined-p
                         canonical
                         canonical-confinement-roots)
                  (record-diagnostic
                   :outside-root
                   directory
                   "Skill discovery did not follow a directory outside its configured canonical roots.")
                  (return))
               (let ((identity (namestring canonical)))
                 (when (gethash identity visited)
                   (return))
                 (setf (gethash identity visited) t))
               (multiple-value-bind
                     (files subdirectories exceeded-p entries unresolved-links)
                   (handler-case
                       (skill--directory-entries-bounded
                        directory
                        (- max-entries entry-count))
                     (error (condition)
                       (record-diagnostic
                        :scan-error
                        directory
                        (format nil
                                "Could not inspect skill directory: ~A"
                                condition))
                       (values nil nil nil 0 nil)))
                 (incf entry-count entries)
                 (when exceeded-p
                   (stop-scan
                    :scan-entry-limit
                    directory
                    (format nil
                            "Skill scan reached its ~D entry limit."
                            max-entries))
                   (return))
                 (dolist (link unresolved-links)
                   (if (skill-source-pathname-p link)
                       (push link paths)
                       (record-diagnostic
                        :scan-error
                        link
                        "Could not resolve symbolic link during skill discovery.")))
                 (dolist (file files)
                   (when (skill-source-pathname-p file)
                     (push file paths)))
                 (cond
                   ((< depth max-depth)
                    (dolist (subdirectory subdirectories)
                      (walk subdirectory (1+ depth))))
                   (subdirectories
                    (record-diagnostic
                     :scan-depth-limit
                     directory
                     (format nil
                             "Skill scan did not descend beyond depth ~D."
                             max-depth)))))))))
      (if (uiop:directory-exists-p root)
          (let ((resolved-root
                  (handler-case
                      (truename root)
                    (error (condition)
                      (record-diagnostic
                       :scan-error
                       root
                       (format nil
                               "Could not resolve skill root: ~A"
                               condition))
                      nil))))
             (when resolved-root
               (setf canonical-root
                     (uiop:ensure-directory-pathname resolved-root)
                     canonical-confinement-roots
                     (skill--canonical-roots confinement-roots))
               (pushnew canonical-root
                        canonical-confinement-roots
                        :test #'uiop:pathname-equal)
               (walk root 0)))
          (record-diagnostic :missing-root
                             root
                             "Skill root does not exist.")))
    (values (sort (remove-duplicates paths :test #'equal)
                  #'skill--definition-pathname<)
            (nreverse diagnostics)
            directory-count
            entry-count)))


