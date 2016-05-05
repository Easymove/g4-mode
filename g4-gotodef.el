;;; ----------------------------------------------
;;; Identifiers resolution utils
;;; ----------------------------------------------

(defvar *back-positions* nil)

(defun g4-goto-definition ()
  (interactive)
  (let* ((requested-name (name-at-point))
         (requested-rule (find-def requested-name)))
    (save-position)
    (goto-node requested-rule)))


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


;;; save and restore positions
(defun g4-restore-position ()
  (interactive)
  (if *back-positions*
      (goto-char (pop *back-positions*))
    (message "no goback information.")))


(defun save-position ()
  (push (point) *back-positions*))


;;; GOTOs commands
(defun goto-node (node)
  (when (typep node 'position-mixin)
    (let ((pos (spos node)))
      (goto-point (line pos) (column pos)))))


(defun goto-point (line column)
  (let ((i 0))
    (goto-line line)
    (while (< i column)
      (forward-char)
      (incf i))))
