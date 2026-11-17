(in-package #:cl-skills)

(-> skill--catalog-prefix () string)
(defun skill--catalog-prefix ()
  "Return the fixed model-visible skill catalog introduction."
  (format nil
          "## Skills~2%A skill is a reusable instruction set stored in native SKILL.sexp or standard SKILL.md. The entries below contain names and descriptions only. A description that ends in ... is shortened to keep this catalog bounded.~2%### Available skills~%"))

(-> skill--catalog-guidance () string)
(defun skill--catalog-guidance ()
  "Return concise skill selection and progressive-disclosure guidance."
  (format nil
          "~%### Skill rules~%When a task names a listed skill or clearly matches its description, select that skill by exact name through the host application before other task actions. Select every applicable skill. Treat selected instructions as request-local unless the host application documents another lifetime.~2%Before acting, read every selected instruction body completely from request-local context. Resolve linked relative paths from the source file's directory and load only resources needed for the task. Prefer provided scripts and assets. If a skill cannot be read or applied, state that briefly and continue with the best fallback."))

(-> skill--catalog-line (skill-metadata &key (:description (option string)))
    string)
(defun skill--catalog-line (metadata &key description)
  "Render one METADATA line with an optional DESCRIPTION.

The line leaves the source path out: a host selects a skill by name, and the
budget goes to the descriptions a selection is made from."
  (format nil "- ~A~@[: ~A~]" (skill-metadata-name metadata) description))

(-> skill--catalog-description (skill-metadata (integer 0)) (option string))
(defun skill--catalog-description (metadata character-limit)
  "Return METADATA's description in at most CHARACTER-LIMIT characters.

A longer description is cut and ends in three dots. Return NIL when the limit
has no room for any of its text."
  (let ((description (skill-metadata-description metadata)))
    (cond
      ((<= (length description) character-limit)
       description)
      ((> character-limit 3)
       (concatenate 'string
                    (string-right-trim
                     " " (subseq description 0 (- character-limit 3)))
                    "..."))
      (t
       nil))))

(-> skill--catalog-omission-line ((integer 1)) string)
(defun skill--catalog-omission-line (count)
  "Render a notice that COUNT metadata entries did not fit."
  (format nil
          "- ~D additional skill~:P omitted by the catalog character budget."
          count))

(-> skill--catalog-compose
    (list (integer 0) &key (:prefix string) (:guidance string))
    string)
(defun skill--catalog-compose (lines omitted-count &key prefix guidance)
  "Compose catalog LINES and OMITTED-COUNT within host protocol sections."
  (with-output-to-string (stream)
    (write-string prefix stream)
    (if lines
        (loop for line in lines
              do (write-string line stream)
                 (terpri stream))
        (when (zerop omitted-count)
          (write-string "- No skills discovered." stream)))
    (when (plusp omitted-count)
      (write-string (skill--catalog-omission-line omitted-count) stream)
      (terpri stream))
    (write-string guidance stream)))

(-> skill-catalog-render
    (skill-catalog &key (:character-budget (integer 1))
                        (:description-character-limit (integer 0))
                        (:prefix string)
                        (:guidance string))
    (values string (integer 0) (integer 0)))
(defun skill-catalog-render
    (catalog
     &key
       (character-budget *skill-catalog-character-budget*)
       (description-character-limit *skill-catalog-description-character-limit*)
       (prefix (skill--catalog-prefix))
       (guidance (skill--catalog-guidance)))
  "Render bounded CATALOG metadata.

Return the rendered text, included metadata count, and omitted metadata count.
PREFIX and GUIDANCE delimit the host-specific catalog protocol. Names come
first: a skill is listed while its name fits CHARACTER-BUDGET. The listed
descriptions then share what is left: each is cut to the longest common length
that fits, at most DESCRIPTION-CHARACTER-LIMIT characters. The function never
retains a skill instruction string."
  (let* ((skills (skill-catalog-skills catalog))
         (minimum
           (skill--catalog-compose
            nil
            (if skills (length skills) 0)
            :prefix   prefix
            :guidance guidance)))
    (when (> (length minimum) character-budget)
      (error 'skill-catalog-render-error
             :message
             (format nil
                     "Skill catalog budget ~D is below the required ~D characters."
                     character-budget
                     (length minimum))
             :character-budget character-budget
             :minimum-required (length minimum)))
    (if (null skills)
        (values minimum 0 0)
        (let ((selected nil)
              (lines    nil))
          (dolist (metadata skills)
            (let* ((candidate-lines
                     (append lines
                             (list (skill--catalog-line metadata))))
                   (omitted
                     (- (length skills) (length candidate-lines)))
                   (rendered
                     (skill--catalog-compose
                      candidate-lines omitted
                      :prefix   prefix
                      :guidance guidance)))
              (when (<= (length rendered) character-budget)
                (setf selected
                      (append selected (list metadata))
                      lines candidate-lines))))
          (let ((omitted (- (length skills) (length selected))))
            (flet ((render (character-limit)
                     (skill--catalog-compose
                      (mapcar (lambda (metadata)
                                (skill--catalog-line
                                 metadata
                                 :description (skill--catalog-description
                                               metadata character-limit)))
                              selected)
                      omitted
                      :prefix   prefix
                      :guidance guidance)))
              (values (loop for character-limit
                              downfrom description-character-limit to 0
                            for rendered = (render character-limit)
                            when (<= (length rendered) character-budget)
                              return rendered)
                      (length selected)
                      omitted)))))))
