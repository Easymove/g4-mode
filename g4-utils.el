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

(defmethod traverse-next ((node complex-entity-node))
  (node-value node))

(defmethod traverse-next (node)
  nil)

(defun node-string-name (node)
  (cond
   ((typep node 'named-mixin)
    (node-string-name (node-name node)))
   ((typep node 'identifier)
    (name node))
   ((typep node 'literal)
    (value node))
   (t node)))
