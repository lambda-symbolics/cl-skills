(in-package #:cl-skills)

(defun skill--parse-source-definition
    (pathname
     &key
       (instruction-character-limit *skill-instruction-character-limit*)
       (file-character-limit *skill-file-character-limit*)
       roots
       cache-root)
  "Read PATHNAME according to its exact supported skill source format."
  (ecase (skill-source-format-for-pathname pathname)
    (:native
     (skill--parse-definition
      pathname
      :instruction-character-limit instruction-character-limit
      :file-character-limit file-character-limit
      :roots roots))
    (:agent-skill
     (skill--parse-agent-definition
      pathname
      :instruction-character-limit instruction-character-limit
      :file-character-limit file-character-limit
      :roots roots
      :cache-root cache-root))))

(-> skill--load-metadata
    (pathname pathname (integer 0)
     &key (:file-character-limit (integer 1))
          (:aggregate-limit-p boolean)
          (:confinement-roots list)
          (:cache-root (option pathname)))
    (values (option skill-metadata)
            (option skill-diagnostic)
            (integer 0)))
(defun skill--load-metadata
    (pathname root root-index
     &key
       (file-character-limit *skill-file-character-limit*)
       aggregate-limit-p
       confinement-roots
       cache-root)
  "Return metadata or one typed diagnostic for PATHNAME."
  (handler-case
      (let ((source-format (skill-source-format-for-pathname pathname)))
        (multiple-value-bind
              (name description instructions canonical-pathname
               source-character-count)
            (skill--parse-source-definition
             pathname
             :instruction-character-limit *skill-file-character-limit*
             :file-character-limit file-character-limit
             :roots confinement-roots
             :cache-root cache-root)
          (declare (ignore instructions))
          (values
           (make-instance 'skill-metadata
                          :name name
                          :description description
                          :pathname pathname
                          :canonical-pathname canonical-pathname
                          :root root
                          :confinement-roots confinement-roots
                          :root-index root-index
                          :source-format source-format
                          :cache-root
                          (and (eq source-format ':agent-skill)
                               cache-root))
           nil
           source-character-count)))
    (skill--definition-error (condition)
      (values
       nil
       (skill--diagnostic
        :kind
        (if (and aggregate-limit-p
                 (eq (skill--definition-error-kind condition)
                     ':file-too-large))
            ':scan-character-limit
            (skill--definition-error-kind condition))
        :pathname pathname
        :root-index root-index
        :message
        (if (and aggregate-limit-p
                 (eq (skill--definition-error-kind condition)
                     ':file-too-large))
            "The aggregate skill discovery character budget was exhausted."
            (skill--definition-error-message condition)))
       (skill--definition-error-source-character-count condition)))))


;;;; -- Catalog Assembly and Fresh Reads --

(-> skill-catalog-discover
    (list
     &key (:max-depth (integer 0))
          (:max-directories (integer 1))
          (:max-entries (integer 1))
          (:max-characters (integer 1))
          (:cache-root (option pathname)))
    skill-catalog)
(defun skill-catalog-discover
    (roots
     &key
       (max-depth *skill-scan-depth-limit*)
       (max-directories *skill-scan-directory-limit*)
       (max-entries *skill-scan-entry-limit*)
       (max-characters *skill-discovery-character-limit*)
       cache-root)
  "Discover skills beneath ordered ROOTS, with earlier roots taking precedence."
  (let ((roots (mapcar (lambda (root)
                         (uiop:ensure-directory-pathname (pathname root)))
                       roots))
        (skills nil)
        (diagnostics nil)
        (reserved (make-hash-table :test #'equal))
        (remaining-directories max-directories)
        (remaining-entries max-entries)
        (remaining-characters max-characters)
        (character-budget-exhausted-p nil))
    (loop for root in roots
          for root-index from 0
          do
             (when (or (zerop remaining-directories)
                       (zerop remaining-entries)
                       (zerop remaining-characters))
               (push
                (skill--diagnostic
                 :kind
                 (cond
                   ((zerop remaining-directories)
                    ':scan-directory-limit)
                   ((zerop remaining-entries)
                    ':scan-entry-limit)
                   (t
                    ':scan-character-limit))
                 :pathname root
                 :root-index root-index
                 :message
                 "The aggregate skill discovery budget was exhausted before this root.")
                diagnostics)
               (loop-finish))
              (multiple-value-bind
                    (pathnames scan-diagnostics directories entries)
                  (skill--scan-root
                   root
                   root-index
                   :max-depth max-depth
                   :max-directories remaining-directories
                   :max-entries remaining-entries
                   :confinement-roots roots)
                (decf remaining-directories directories)
                (decf remaining-entries entries)
                (dolist (diagnostic scan-diagnostics)
                  (push diagnostic diagnostics))
                (dolist (pathname pathnames)
                  (when (zerop remaining-characters)
                    (push
                     (skill--diagnostic
                      :kind ':scan-character-limit
                      :pathname pathname
                      :root-index root-index
                      :message
                      "The aggregate skill discovery character budget was exhausted.")
                     diagnostics)
                    (setf character-budget-exhausted-p t)
                    (loop-finish))
                  (let ((file-character-limit
                          (min *skill-file-character-limit*
                               remaining-characters)))
                    (multiple-value-bind
                          (metadata diagnostic source-character-count)
                        (skill--load-metadata
                         pathname
                         root
                         root-index
                         :file-character-limit file-character-limit
                         :aggregate-limit-p
                         (<= remaining-characters
                             *skill-file-character-limit*)
                         :confinement-roots roots
                         :cache-root cache-root)
                      (decf remaining-characters
                            (min remaining-characters
                                 source-character-count))
                      (when (and diagnostic
                                 (eq
                                  (skill-diagnostic-kind diagnostic)
                                  ':scan-character-limit))
                        (setf character-budget-exhausted-p t))
                      (cond
                        (diagnostic
                         (push diagnostic diagnostics))
                        ((gethash (skill-metadata-name metadata) reserved)
                         (push
                          (skill--diagnostic
                           :kind ':shadowed
                           :pathname pathname
                           :root-index root-index
                           :message
                           (format nil
                                   "Skill ~A is blocked by earlier ~A."
                                   (skill-metadata-name metadata)
                                   (namestring
                                    (gethash
                                     (skill-metadata-name metadata)
                                     reserved))))
                          diagnostics))
                        (t
                         (setf (gethash (skill-metadata-name metadata) reserved)
                               pathname)
                         (push metadata skills))))))
                (when character-budget-exhausted-p
                  (loop-finish))))
    (make-instance 'skill-catalog
                   :skills (nreverse skills)
                   :diagnostics (nreverse diagnostics))))

(-> skill-metadata-discover
    (pathname pathname &key (:root-index (integer 0))
                           (:cache-root (option pathname)))
    (values (option skill-metadata) (option skill-diagnostic)))
(defun skill-metadata-discover (pathname root &key (root-index 0) cache-root)
  "Read one exact supported source beneath ROOT without enumerating a catalog.
Return fresh metadata or a structured diagnostic. Confinement and parser limits
are identical to catalog discovery; no executable code is loaded."
  (let ((root (uiop:ensure-directory-pathname root)))
    (if (skill-source-format-for-pathname pathname)
        (multiple-value-bind (metadata diagnostic characters)
            (skill--load-metadata pathname root root-index
                                  :confinement-roots (list root)
                                  :cache-root cache-root)
          (declare (ignore characters))
          (values metadata diagnostic))
        (values nil (skill--diagnostic
                     :kind ':invalid-source :pathname pathname :root-index root-index
                     :message "The pathname does not name a supported skill source.")))))

(-> skill-catalog-find (skill-catalog string) (option skill-metadata))
(defun skill-catalog-find (catalog name)
  "Return the selected skill named NAME from CATALOG, if present."
  (find name
        (skill-catalog-skills catalog)
        :key #'skill-metadata-name
        :test #'string=))

(-> skill-metadata-read (skill-metadata) string)
(defun skill-metadata-read (metadata)
  "Read and return METADATA's complete current instruction string."
  (let ((pathname (skill-metadata-pathname metadata)))
    (handler-case
        (multiple-value-bind
              (name description instructions canonical-pathname
               source-character-count)
            (skill--parse-source-definition
             pathname
             :roots (skill-metadata--confinement-roots metadata)
             :cache-root (skill-metadata-cache-root metadata))
          (declare (ignore name description canonical-pathname
                           source-character-count))
          instructions)
      (skill--definition-error (condition)
        (if (eq (skill--definition-error-kind condition) ':file-too-large)
            (error 'skill-body-too-large
                   :message (skill--definition-error-message condition)
                   :pathname pathname
                   :cause condition
                   :character-limit
                   (min *skill-file-character-limit*
                        *skill-instruction-character-limit*))
            (error 'skill-read-error
                   :message (skill--definition-error-message condition)
                   :pathname pathname
                   :cause condition)))
      (error (condition)
        (error 'skill-read-error
               :message (format nil
                                "Could not read selected skill ~A: ~A"
                                (namestring pathname)
                                condition)
               :pathname pathname
               :cause condition)))))


