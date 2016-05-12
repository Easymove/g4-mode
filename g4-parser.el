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

(defclass entity-node-mixin (node value-mixin)
  ((modifier :accessor modifier
             :initarg :mod
             :initform nil)))

(defclass entity-node (entity-node-mixin)
  ())

(defclass complex-entity-node (entity-node-mixin)
  ())

(defclass sort-node (node named-mixin)
  ())


;;; Parser
(defun parser (lexems)
  (grammar lexems))

(defun grammar (lexems)
  (when lexems
    (multiple-value-bind (grammar-name rest) (grammar-def lexems)
      (multiple-value-bind (rules rest1) (rule-list rest)
        (unless rest1
          (make-instance 'grammar-node
                         :name grammar-name
                         :members rules))))))

(defun grammar-def (lexems)
  (if (and (typep (car lexems) 'identifier)
           (equal (name (car lexems)) "grammar")
           (typep (second lexems) 'identifier)
           (typep (third lexems) 'semi-colon))
      (values (name (second lexems)) (cdddr lexems))
    (values "_default_" lexems)))

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
    (cond
     (ent-list
      (values (make-instance 'command-node
                             :members ent-list
                             :start (spos (car ent-list))
                             :end (epos (car (last ent-list))))
              rest))
     ((and (null ent-list)
           (typep (car rest) 'pipe))
      (values (make-instance 'empty-command-node
                             :start (spos (car rest))
                             :end (spos (car rest))) rest))
     (t (values nil rest)))))

(defun entity-list (lexems)
  (when lexems
    (multiple-value-bind (entity rest) (entity lexems)
      (if entity
          (multiple-value-bind (ent-list rest1) (entity-list rest)
            (values (cons entity ent-list) rest1))
        (values nil rest)))))

(defun entity (lexems)
  (when lexems
    (cond ((or (typep (car lexems) 'identifier)
               (typep (car lexems) 'literal))
           (let ((ent (make-instance 'entity-node
                                     :value (car lexems)
                                     :start (spos (car lexems))
                                     :end (epos (car lexems))
                                     :mod (cond ((typep (cadr lexems) 'star)
                                                 :none-or-many)
                                                ((typep (cadr lexems) 'plus)
                                                 :one-or-many)
                                                ((typep (cadr lexems) 'interrogation)
                                                 :optional)
                                                (t :default)))))
             (values ent (if (eq (modifier ent) :default) (cdr lexems) (cddr lexems)))))
          ((typep (car lexems) 'o-parenthesis)
           (multiple-value-bind (ent-list rest) (entity-list (cdr lexems))
             (if (and ent-list
                      (typep (car rest) 'c-parenthesis))
                 (let ((ent (make-instance 'complex-entity-node
                                           :value ent-list
                                           :start (spos (car lexems))
                                           :end (epos (car rest))
                                           :mod (cond ((typep (cadr rest) 'star)
                                                       :none-or-many)
                                                      ((typep (cadr rest) 'plus)
                                                       :one-or-many)
                                                      ((typep (cadr rest) 'interrogation)
                                                       :optional)
                                                      (t :default)))))
                   (values ent (if (eq (modifier ent) :default) (cdr rest) (cddr rest))))
               (error "ENTITY: close parenthesis expected. line: %s column: %s"
                      (line (spos (car rest)))
                      (column (spos (car rest)))))))
          (t (values nil lexems)))))

