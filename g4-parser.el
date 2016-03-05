;;; -------------------------------------------------------
;;; basic mixins
;;; -------------------------------------------------------
(defclass members-mixin ()
  ((members :accessor node-members
            :initarg :members
            :initform nil)))

(defclass named-mixin ()
  ((name :accessor node-name
         :initarg :name
         :initform nil)))

(defclass position-mixin ()
  ((start-position :accessor spos
                   :initarg :start
                   :type position)
   (end-position :accessor epos
                 :initarg :end
                 :type position)))

(defclass value-mixin ()
  ((value :accessor node-value
          :initarg :value
          :initform nil)))

;;; Nodes
(defclass node (position-mixin)
  ())

(defclass grammar-node (node named-mixin members-mixin)
  ())

(defclass rule-node (node named-mixin members-mixin)
  ())

(defclass command-node (node members-mixin)
  ())

(defclass empty-command-node (command-node)
  ())

(defclass entity-node (node value-mixin)
  ())

(defclass sort-node (node named-mixin)
  ())


;;; Parser
(defun parser (lexems)
  (grammar lexems))

(defun grammar (lexems)
  (rule-list lexems))

(defun rule-list (lexems)
  (when lexems
    (multiple-value-bind (rule rest) (rule lexems)
      (if rule
          (multiple-value-bind (rl-list rest1) (rule-list rest)
            (values (cons rule rl-list) rest1))
        (values nil rest)))))

(defun rule (lexems)
  (when lexems
    (multiple-value-bind (sort rest) (sort-name lexems)
      (unless (typep (car rest) 'colon)
        (error "RULE: colon expected. line: %s column: %s"
               (line (spos (car rest)))
               (column (spos (car rest)))))
      (multiple-value-bind (comm-list rest1) (command-list (cdr rest))
        (unless (typep (car rest1) 'semi-colon)
          (error "RULE: semi-colon expected. line: %s column: %s"
                 (line (epos (car (last comm-list))))
                 (column (epos (car (last comm-list))))))
        (values (make-instance 'rule-node
                               :name sort
                               :members comm-list
                               :start (spos sort)
                               :end (epos (car rest1)))
                (cdr rest1))))))

(defun sort-name (lexems)
  (when lexems
    (if (typep (car lexems) 'identifier)
        (values (make-instance 'sort-node
                               :name (name (car lexems))
                               :start (spos (car lexems))
                               :end (epos (car lexems)))
                (cdr lexems))
      (error "SORT: identifier expected. line: %s column: %s"
             (line (spos (car lexems)))
             (column (spos (car lexems)))))))

(defun command-list (lexems)
  (when lexems
    (multiple-value-bind (command rest) (command lexems)
      (if command
          (multiple-value-bind (comm-list rest1) (command-list-rest rest)
            (values (cons command comm-list) rest1))
        (values nil rest)))))

(defun command-list-rest (lexems)
  (when lexems
    (if (typep (car lexems) 'pipe)
        (multiple-value-bind (command rest) (command (cdr lexems))
          (if command
              (multiple-value-bind (comm-list rest1) (command-list-rest rest)
                (values (cons command comm-list) rest1))
            (values (list (make-instance 'empty-command-node
                                         :start (spos (car rest))
                                         :end (spos (car rest))))
                    rest)))
      (values nil lexems))))

(defun command (lexems)
  (multiple-value-bind (ent-list rest) (entity-list lexems)
    (if ent-list
        (values (make-instance 'command-node
                               :members ent-list
                               :start (spos (car ent-list))
                               :end (epos (car (last ent-list))))
                rest)
      (values nil rest))))

(defun entity-list (lexems)
  (when lexems
    (multiple-value-bind (entity rest) (entity lexems)
      (if entity
          (multiple-value-bind (ent-list rest1) (entity-list rest)
            (values (cons entity ent-list) rest1))
        (values nil rest)))))

(defun entity (lexems)
  (when lexems
    (if (or (typep (car lexems) 'identifier)
            (typep (car lexems) 'literal))
        (values (make-instance 'entity-node
                               :value (car lexems)
                               :start (spos (car lexems))
                               :end (epos (car lexems)))
                (cdr lexems))
      (values nil lexems))))

