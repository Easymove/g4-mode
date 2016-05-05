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

;;; -------------------------------------------------
;;; grammar vizualizer
;;; -------------------------------------------------

(defun g4-dump-to-dot ()
  (interactive)
  (let ((dot-str "")
        (id 0) (edges) (visited))
    (cl-labels ((%get-id ()
                         (incf id))
                (%visited-p (node)
                            (cl-member node visited :test #'equal))
                (%add (str)
                      (setf dot-str (concat dot-str str)))
                (%graph-header (name)
                               (%add (format "digraph %s {\n" name)))
                (%footer ()
                         (%add "}\n"))
                (%build-rule (rule)
                             (if (%visited-p rule)
                                 (let ()
                                   ;; TODO: Link with existing node
                                   )
                                 (let ((cur-id (%get-id)))
                                   (push rule visited)
                                   (%add (format "%s [label=\"%s\", color=\"blue\", fillcolor=\"#89EED9\"];\n" cur-id (node-string-name rule)))
                                   (mapc (lambda (n)
                                           (let ((next-id (%build-rule n)))
                                             (push (cons cur-id next-id) edges)))
                                         (get-next-rules rule))
                                   cur-id)))
                (%build-edges ()
                              (mapc (lambda (pair)
                                      (%add (format "%s -> %s;\n" (car pair) (cdr pair))))
                                    edges))
                (%build-graph (name next)
                              (%graph-header name)
                              (%build-rule (car next))
                              (%build-edges)
                              (%footer)))
      (%build-graph (node-string-name *current-grammar*) (traverse-next *current-grammar*)))

    (with-temp-buffer
      (insert dot-str)
      (write-region (point-min) (point-max) "graph.dot" nil nil nil t))

    dot-str))


(defun get-next-rules (rule)
  (let ((next-names))
    (traverse rule
              (lambda (n)
                (when (and (typep n 'entity-node)
                           (typep (node-value n) 'identifier))
                  (push (node-string-name (node-value n)) next-names))))
    (reverse (mapcar #'lookup-name next-names))))


(defun lookup-name (name)
  (catch 'res
    (traverse *current-grammar*
              (lambda (node)
                (when (and (typep node 'rule-node)
                           (equal name (node-string-name node)))
                  (throw 'res node))))
    name))
