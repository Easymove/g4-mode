;;; ----------------------------------------------
;;; Identifiers resolution utils
;;; ----------------------------------------------

(defun g4-goto-definition ()
  (interactive)
  (let* ((requested-name (name-at-point))
         (requested-rule (find-def requested-name)))
    (when (typep requested-rule 'position-mixin)
      (let ((i 0) (pos (spos requested-rule)))
        (goto-line (line pos))
        (while (< i (column pos))
          (forward-char)
          (incf i))))))


(defun name-at-point ()
  (thing-at-point 'word))


(defun find-def (name)
  (let ((res-rule))
    (traverse *current-grammar*
              (lambda (node)
                (when (and (or (typep node 'grammar-node)
                               (typep node 'rule-node))
                           (equal name (node-string-name node)))
                  (setf res-rule node))))
    res-rule))
