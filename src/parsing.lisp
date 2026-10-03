(in-package #:cl-skills)

;;;; -- Native Skill Form --

(-> skill--definition-fail (skill-diagnostic-kind string &rest t) t)
(defun skill--definition-fail (kind control &rest arguments)
  "Signal one internal skill definition failure of KIND."
  (error 'skill--definition-error
         :kind kind
         :source-character-count *skill-definition-source-character-count*
         :message (apply #'format nil control arguments)))

(-> skill--read-file-bounded
    (pathname (integer 1) &key (:roots list))
    (values string pathname t))
(defun skill--read-file-bounded (pathname character-limit &key roots)
  "Read one root-confined regular PATHNAME with a stable filesystem identity.

Return the bounded UTF-8 source, canonical pathname, and the EQUAL-comparable
filesystem identity of the file read. Opening never blocks, so a FIFO or other
non-regular candidate cannot stall discovery. When ROOTS is non-NIL, the file
must resolve beneath one of those configured roots."
  (handler-case
      (let* ((canonical (truename pathname))
             (canonical-roots (skill--canonical-roots roots)))
        (when (and roots
                   (not
                    (skill--canonical-pathname-confined-p
                     canonical
                     canonical-roots)))
          (skill--definition-fail
           :outside-root
           "The skill source resolves outside its configured canonical roots."))
        (let* ((expected
                 (ls-compat.posix:file-information pathname :follow-links-p t))
               (expected-identity
                 (ls-compat.posix:file-information-identity expected))
               (stream nil))
          (unless (eq (ls-compat.posix:file-information-kind expected) ':file)
            (skill--definition-fail
             :not-regular-file
             "The skill source must resolve to a regular file."))
          (unwind-protect
                (multiple-value-bind (opened-stream opened)
                    (handler-case
                        (ls-compat.posix:open-regular-file
                         pathname
                         :follow-links-p t
                         :element-type 'character
                         :external-format ':utf-8)
                      (ls-compat.posix:not-regular-file ()
                        (skill--definition-fail
                         :not-regular-file
                         "The skill source must resolve to a regular file.")))
                  (setf stream opened-stream)
                  (let* ((opened-identity
                           (ls-compat.posix:file-information-identity opened))
                         (current-canonical (truename pathname))
                         (current
                           (ls-compat.posix:file-information
                            pathname :follow-links-p t)))
                    (unless (and (equal opened-identity expected-identity)
                                 (equal opened-identity
                                        (ls-compat.posix:file-information-identity
                                         current)))
                      (skill--definition-fail
                       :identity-changed
                       "The skill source changed identity while it was being opened."))
                    (when (and roots
                               (not
                                (skill--canonical-pathname-confined-p
                                 current-canonical
                                 canonical-roots)))
                      (skill--definition-fail
                       :outside-root
                       "The skill source resolves outside its configured canonical roots."))
                    ;; A decoding or stream failure does not report its
                    ;; partial progress. Charge the complete allowance
                    ;; until a successful read supplies the exact count.
                    (setf *skill-definition-source-character-count*
                          character-limit)
                    (let* ((buffer (make-string (1+ character-limit)))
                           (count (read-sequence buffer stream)))
                      (setf *skill-definition-source-character-count* count)
                      (when (> count character-limit)
                        (skill--definition-fail
                         :file-too-large
                         "The skill source exceeds the ~D-character file limit."
                         character-limit))
                      (values (subseq buffer 0 count)
                              current-canonical
                              opened-identity))))
            (when stream
              (close stream)))))
    (skill--definition-error (condition)
      (error condition))
    (error (condition)
      (skill--definition-fail
       :read-error
       "Could not read the skill source: ~A"
       condition))))

(-> skill--source-grammar () source-grammar)
(defun skill--source-grammar ()
  "Return the bounded native data grammar one SKILL.sexp may use.

The grammar is rebuilt for every read so that a live change to the keyword or
bound policy takes effect without reloading this file. Charging list length to
the depth budget keeps one bound over both nesting and length, and withholding
COMMON-LISP from the reader package keeps a bare symbol from naming anything."
  (make-source-grammar
   :label "SKILL.sexp"
   :keywords *skill-native-keywords*
   :maximum-depth *skill-form-depth-limit*
   :maximum-nodes *skill-form-node-limit*
   :list-tails-increase-depth-p t
   :common-lisp-symbols-permitted-p nil))

(-> skill--source-diagnostic-kind (sexp-config-error) skill-diagnostic-kind)
(defun skill--source-diagnostic-kind (condition)
  "Return the skill diagnostic kind reporting CONDITION."
  (case (sexp-config-error-kind condition)
    (:unknown-field ':unknown-field)
    (:data-too-deep ':data-too-deep)
    (:data-too-large ':data-too-large)
    ((:invalid-structure :invalid-value) ':invalid-structure)
    (otherwise ':invalid-syntax)))

(-> skill--read-one-form (string) t)
(defun skill--read-one-form (source)
  "Read and return exactly one native form from bounded SOURCE."
  (handler-case
      (read-source source (skill--source-grammar))
    (sexp-config-error (condition)
      (skill--definition-fail (skill--source-diagnostic-kind condition)
                              "~A"
                              (sexp-config-error-message condition)))))

(-> skill--validate-tree (t) null)
(defun skill--validate-tree (form)
  "Reject improper, circular, shared, deep, or oversized FORM structure."
  (handler-case
      (progn (validate-tree form (skill--source-grammar)) nil)
    (sexp-config-error (condition)
      (skill--definition-fail (skill--source-diagnostic-kind condition)
                              "~A"
                              (sexp-config-error-message condition)))))

(-> skill--normalize-description (string) string)
(defun skill--normalize-description (description)
  "Return DESCRIPTION with whitespace runs collapsed for catalog display."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return #\Page)
   (with-output-to-string (stream)
     (let ((pending-space-p nil)
           (wrote-p nil))
       (loop for character across description
             do (if (find character
                          '(#\Space #\Tab #\Newline #\Return #\Page))
                    (when wrote-p
                      (setf pending-space-p t))
                    (progn
                      (when pending-space-p
                        (write-char #\Space stream))
                      (write-char character stream)
                      (setf pending-space-p nil
                            wrote-p t))))))))

(-> skill--validate-name (t) string)
(defun skill--validate-name (name)
  "Return validated skill NAME."
  (unless (stringp name)
    (skill--definition-fail
     :invalid-name
     "The :name value must be a string."))
  (when (zerop (length name))
    (skill--definition-fail
     :invalid-name
     "The :name value must not be empty."))
  name)

(-> skill--validate-description (t) string)
(defun skill--validate-description (description)
  "Return a validated single-line skill DESCRIPTION."
  (unless (stringp description)
    (skill--definition-fail
     :invalid-description
     "The :description value must be a string."))
  (let ((description (skill--normalize-description description)))
    (when (zerop (length description))
      (skill--definition-fail
       :invalid-description
       "The :description value must not be empty."))
    (when (> (length description) *skill-description-character-limit*)
      (skill--definition-fail
       :invalid-description
       "Skill description exceeds ~D characters."
       *skill-description-character-limit*))
    description))

(-> skill--validate-instructions
    (t (integer 1) &key (:allow-empty-p boolean))
    string)
(defun skill--validate-instructions
    (instructions character-limit &key allow-empty-p)
  "Return validated skill INSTRUCTIONS without modifying their contents."
  (unless (stringp instructions)
    (skill--definition-fail
     :invalid-instructions
     "The :instructions value must be a string."))
  (when (and (not allow-empty-p)
             (zerop
              (length
               (string-trim
                '(#\Space #\Tab #\Newline #\Return #\Page)
                instructions))))
    (skill--definition-fail
     :invalid-instructions
     "The :instructions value must not be empty."))
  (when (> (length instructions) character-limit)
    (skill--definition-fail
     :file-too-large
     "Skill instructions exceed the ~D-character limit."
     character-limit))
  instructions)

(-> skill--parse-native-source
    (string
     &key (:instruction-character-limit (integer 1))
          (:allow-empty-instructions-p boolean))
    (values string string string))
(defun skill--parse-native-source
    (source
     &key
       (instruction-character-limit *skill-instruction-character-limit*)
       allow-empty-instructions-p)
  "Validate one native Autolith skill form from bounded SOURCE."
  (let ((form (skill--read-one-form source)))
    (unless (and (consp form)
                 (eq (first form) ':autolith-skill))
      (skill--definition-fail
       :invalid-structure
       "SKILL.sexp must begin with :autolith-skill."))
    (let ((fields (rest form))
          (values (make-hash-table :test #'eq)))
      (loop while fields
            do
               (unless (rest fields)
                 (skill--definition-fail
                  :invalid-structure
                  "SKILL.sexp contains a field without a value."))
               (let ((key (first fields))
                     (value (second fields)))
                 (unless (member key
                                 '(:version
                                   :name
                                   :description
                                   :instructions)
                                 :test #'eq)
                   (skill--definition-fail
                    :unknown-field
                    "SKILL.sexp contains unknown field ~S."
                    key))
                 (multiple-value-bind (present-value present-p)
                     (gethash key values)
                   (declare (ignore present-value))
                   (when present-p
                     (skill--definition-fail
                      :duplicate-field
                      "SKILL.sexp contains duplicate field ~S."
                      key)))
                 (setf (gethash key values) value))
               (setf fields (rest (rest fields))))
      (dolist (key '(:version :name :description :instructions))
        (multiple-value-bind (value present-p)
            (gethash key values)
          (declare (ignore value))
          (unless present-p
            (skill--definition-fail
             :missing-field
             "SKILL.sexp requires field ~S."
             key))))
      (let ((version (gethash ':version values))
            (name (skill--validate-name (gethash ':name values)))
            (description
              (skill--validate-description
               (gethash ':description values)))
            (instructions
              (skill--validate-instructions
               (gethash ':instructions values)
               instruction-character-limit
               :allow-empty-p allow-empty-instructions-p)))
        (unless (eql version 1)
          (skill--definition-fail
           :invalid-version
           "SKILL.sexp :version must be the integer 1."))
        (values name description instructions)))))

(-> skill--parse-definition
    (pathname &key (:instruction-character-limit (integer 1))
                   (:file-character-limit (integer 1))
                   (:roots list)
                   (:allow-empty-instructions-p boolean))
    (values string string string pathname (integer 0)))
(defun skill--parse-definition
    (pathname
     &key
       (instruction-character-limit *skill-instruction-character-limit*)
       (file-character-limit *skill-file-character-limit*)
       roots
       allow-empty-instructions-p)
  "Read and validate PATHNAME as one native Autolith skill definition."
  (let ((*skill-definition-source-character-count* 0))
    (multiple-value-bind (source canonical-pathname device inode)
        (skill--read-file-bounded
         pathname
         file-character-limit
         :roots roots)
      (declare (ignore device inode))
      (multiple-value-bind (name description instructions)
          (skill--parse-native-source
           source
           :instruction-character-limit instruction-character-limit
           :allow-empty-instructions-p allow-empty-instructions-p)
        (values name
                description
                instructions
                canonical-pathname
                *skill-definition-source-character-count*)))))


;;;; -- Agent Skills Conversion --

(defparameter *skill-agent-frontmatter-fields*
  '("name" "description" "license" "compatibility" "metadata" "allowed-tools")
  "The complete top-level YAML field vocabulary accepted from SKILL.md.")

(-> skill-source-digest (string) string)
(defun skill-source-digest (source)
  "Return the lowercase SHA-256 digest of SOURCE's exact UTF-8 bytes."
  (string-downcase
   (with-output-to-string (stream)
     (loop for octet across
           (digest-sequence
            ':sha256
            (sb-ext:string-to-octets source :external-format ':utf-8))
           do (format stream "~2,'0X" octet)))))

(-> skill--agent-frontmatter (string) (values string string))
(defun skill--agent-frontmatter (source)
  "Split SOURCE into YAML frontmatter and the exact following Markdown body."
  (let* ((length (length source))
         (opening-start
           (if (and (plusp length)
                    (char= (char source 0) (code-char #xfeff)))
               1
               0)))
    (labels
        ((line-boundaries (start)
           (let* ((newline (position #\Newline source :start start))
                  (raw-end (or newline length))
                  (content-end
                    (if (and (> raw-end start)
                             (char= (char source (1- raw-end)) #\Return))
                        (1- raw-end)
                        raw-end))
                  (next (if newline (1+ newline) length)))
             (values content-end next))))
      (multiple-value-bind (opening-end yaml-start)
          (line-boundaries opening-start)
        (unless (string= source "---"
                         :start1 opening-start
                         :end1 opening-end)
          (skill--definition-fail
           :invalid-syntax
           "SKILL.md must begin with an exact --- delimiter line."))
        (loop with line-start = yaml-start
              while (< line-start length)
              do
                 (multiple-value-bind (line-end line-next)
                     (line-boundaries line-start)
                   (when (string= source "---"
                                  :start1 line-start
                                  :end1 line-end)
                     (return-from skill--agent-frontmatter
                       (values (subseq source yaml-start line-start)
                               (subseq source line-next))))
                   (setf line-start line-next)))
        (skill--definition-fail
         :invalid-syntax
         "SKILL.md requires an exact closing --- delimiter line.")))))

(-> skill--agent-map-insert (hash-table t t) hash-table)
(defun skill--agent-map-insert (map key value)
  "Insert KEY and VALUE into MAP while rejecting duplicate YAML keys."
  (multiple-value-bind (present-value present-p)
      (gethash key map)
    (declare (ignore present-value))
    (when present-p
      (skill--definition-fail
       :duplicate-field
       "SKILL.md contains duplicate YAML key ~S."
       key)))
  (setf (gethash key map) value)
  map)

(-> skill--agent-parse-yaml (string) t)
(defun skill--agent-parse-yaml (source)
  "Parse one YAML 1.2 frontmatter document from SOURCE."
  (handler-case
      (let ((nyaml:*make-map*
              (lambda () (make-hash-table :test #'equal)))
            (nyaml:*map-insert* #'skill--agent-map-insert))
        (nyaml:parse source :schema nyaml:+yaml-12-schema+))
    (skill--definition-error (condition)
      (error condition))
    (error (condition)
      (skill--definition-fail
       :invalid-syntax
       "Could not parse SKILL.md YAML frontmatter: ~A"
       condition))))

(-> skill--agent-required-string
    (hash-table string skill-diagnostic-kind)
    string)
(defun skill--agent-required-string (frontmatter field invalid-kind)
  "Return required string FIELD from FRONTMATTER or signal INVALID-KIND."
  (multiple-value-bind (value present-p)
      (gethash field frontmatter)
    (unless present-p
      (skill--definition-fail
       :missing-field
       "SKILL.md requires frontmatter field ~S."
       field))
    (unless (stringp value)
      (skill--definition-fail
       invalid-kind
       "SKILL.md frontmatter field ~S must be a string."
       field))
    value))

(-> skill--agent-name-character-p (character) boolean)
(defun skill--agent-name-character-p (character)
  "Return true when CHARACTER is permitted in a standard Agent Skill name."
  (or (and (char>= character #\a) (char<= character #\z))
      (and (char>= character #\0) (char<= character #\9))
      (char= character #\-)))

(-> skill-name-valid-p (t) boolean)
(defun skill-name-valid-p (name)
  "Return T when NAME follows the portable Agent Skills naming grammar."
  (and (stringp name)
       (<= 1 (length name) 64)
       (every #'skill--agent-name-character-p name)
       (not (char= (char name 0) #\-))
       (not (char= (char name (1- (length name))) #\-))
       (null (search "--" name))))

(-> skill--validate-agent-name (string pathname) string)
(defun skill--validate-agent-name (name pathname)
  "Return validated standard Agent Skill NAME for source PATHNAME."
  (unless (skill-name-valid-p name)
    (skill--definition-fail
     :invalid-name
     "SKILL.md name must use 1-64 lowercase ASCII letters, digits, or single hyphens."))
  (let* ((directory
           (pathname-directory
            (uiop:pathname-directory-pathname pathname)))
         (parent (first (last directory))))
    (unless (and (stringp parent) (string= name parent))
      (skill--definition-fail
       :invalid-name
       "SKILL.md name ~S must match its parent directory ~S."
       name
       parent)))
  name)

(-> skill--validate-agent-frontmatter (t pathname) (values string string))
(defun skill--validate-agent-frontmatter (frontmatter pathname)
  "Return validated name and description from parsed FRONTMATTER."
  (unless (hash-table-p frontmatter)
    (skill--definition-fail
     :invalid-structure
     "SKILL.md YAML frontmatter must be a mapping."))
  (maphash
   (lambda (field value)
     (declare (ignore value))
     (unless (and (stringp field)
                  (member field
                          *skill-agent-frontmatter-fields*
                          :test #'string=))
       (skill--definition-fail
        :unknown-field
        "SKILL.md contains unknown frontmatter field ~S."
        field)))
   frontmatter)
  (dolist (field '("license" "compatibility" "allowed-tools"))
    (multiple-value-bind (value present-p)
        (gethash field frontmatter)
      (when (and present-p (not (stringp value)))
        (skill--definition-fail
         :invalid-structure
         "SKILL.md optional field ~S must be a string."
         field))))
  (multiple-value-bind (metadata present-p)
      (gethash "metadata" frontmatter)
    (when present-p
      (unless (hash-table-p metadata)
        (skill--definition-fail
         :invalid-structure
         "SKILL.md metadata must be a string-to-string mapping."))
      (maphash
       (lambda (key value)
         (unless (and (stringp key) (stringp value))
           (skill--definition-fail
            :invalid-structure
            "SKILL.md metadata must be a string-to-string mapping.")))
       metadata)))
  (let* ((name
           (skill--agent-required-string frontmatter "name" ':invalid-name))
         (description
           (skill--agent-required-string
            frontmatter
            "description"
            ':invalid-description)))
    (when (> (length description) *skill-description-character-limit*)
      (skill--definition-fail
       :invalid-description
       "Skill description exceeds ~D characters."
       *skill-description-character-limit*))
    (values (skill--validate-agent-name name pathname)
            (skill--validate-description description))))

(-> skill--parse-agent-source
    (string pathname &key (:instruction-character-limit (integer 1)))
    (values string string string))
(defun skill--parse-agent-source
    (source pathname
     &key
       (instruction-character-limit *skill-instruction-character-limit*))
  "Parse and validate one complete standard Agent Skill SOURCE."
  (multiple-value-bind (yaml instructions)
      (skill--agent-frontmatter source)
    (multiple-value-bind (name description)
        (skill--validate-agent-frontmatter
         (skill--agent-parse-yaml yaml)
         pathname)
      (values name
              description
              (skill--validate-instructions
               instructions
               instruction-character-limit
               :allow-empty-p t)))))


;;;; -- In-memory Source Validation --

(-> skill-source-validate
    (string pathname
     &key (:file-character-limit (integer 1))
          (:instruction-character-limit (integer 1)))
    (values string string string))
(defun skill-source-validate
    (source pathname
     &key (file-character-limit *skill-file-character-limit*)
          (instruction-character-limit *skill-instruction-character-limit*))
  "Validate SOURCE for its intended PATHNAME without filesystem access.

Return the name, normalized description, and exact instruction body. PATHNAME
selects the exact SKILL.md or SKILL.sexp format; Markdown names must match its
parent directory. Source and instruction bounds use the same policy as file
reads. Invalid definitions signal SKILL-VALIDATION-ERROR."
  (let ((*skill-definition-source-character-count* (length source)))
    (handler-case
        (progn
          (when (> (length source) file-character-limit)
            (skill--definition-fail
             :file-too-large "The skill source exceeds the ~D-character file limit."
             file-character-limit))
          (case (skill-source-format-for-pathname pathname)
            (:native
             (skill--parse-native-source
              source :instruction-character-limit instruction-character-limit))
            (:agent-skill
             (skill--parse-agent-source
              source pathname :instruction-character-limit instruction-character-limit))
            (otherwise
             (skill--definition-fail
              :invalid-structure "A skill source must be named SKILL.md or SKILL.sexp."))))
      (skill--definition-error (condition)
        (error 'skill-validation-error
               :kind (skill--definition-error-kind condition)
               :pathname pathname
               :message (skill--definition-error-message condition))))))
