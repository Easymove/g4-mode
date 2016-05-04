;;; ----------------------------------------------
;;; g4 utilities
;;; ----------------------------------------------

(defun traverse (node func)
  (funcall func node)
  (mapc (lambda (nd)
          (traverse nd func))
        (traverse-next node))
  t)

(defgeneric traverse-next (node)
  "Returns the list of nodes children.")

(defmethod traverse-next ((node members-mixin))
  (node-members node))

(defmethod traverse-next (node)
  nil)

(defun node-string-name (node)
  (when (typep node 'named-mixin)
    (let ((name (node-name node)))
      (cond
       ((typep name 'sort-node)
        (node-string-name name))
       ((typep name 'identifier)
        (name name))
       ((typep name 'literal)
        (value name))
       (t name)))))
