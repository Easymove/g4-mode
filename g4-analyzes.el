;;; ---------------------------------------------
;;; Specific grammars analyzes
;;; ---------------------------------------------

(defvar *unused* nil)
(defvar *undefined* nil)

(defun g4-highlight-unused ()
  (interactive)
  (setf *unused* (get-unused-non-terminals))
  (mapc (lambda (str) (highlight-regexp str 'hi-red-b)) *unused*))


(defun g4-highlight-undefined ()
  (interactive)
  (setf *undefined* (get-undefined-non-terminals))
  (mapc (lambda (str) (highlight-regexp str 'hi-pink)) *undefined*))


(defun g4-unhighlight-all ()
  (interactive)
  (g4-unhighlight-uses)
  (g4-unhighlight-defs))


(defun g4-unhighlight-defs ()
  (interactive)
  (when *undefined*
    (mapc #'unhighlight-regexp *undefined*)))


(defun g4-unhighlight-uses ()
  (interactive)
  (when *unused*
    (mapc #'unhighlight-regexp *unused*)))


(defun get-unused-non-terminals ()
  (multiple-value-bind (uses defs) (get-use-def-non-terminals)
    (cl-set-difference defs uses :test #'equal)))


(defun get-undefined-non-terminals ()
  (multiple-value-bind (uses defs) (get-use-def-non-terminals)
    (cl-set-difference uses defs :test #'equal)))


(defun get-use-def-non-terminals ()
  (let ((defined)
        (used))
    ;; collect all uses and definitions
    (traverse *current-grammar*
              (lambda (node)
                (cond
                  ((typep node 'rule-node)
                   (push (node-string-name node) defined))
                  ((typep node 'entity-node)
                   (when (typep (node-value node) 'identifier)
                     (pushnew (node-string-name (node-value node)) used :test #'equal))))))
    ;; (message "USES: %s; DEFS: %s" used defined)
    (values used defined)))
