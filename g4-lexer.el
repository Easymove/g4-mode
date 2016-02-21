;;; ---------------------------------------------------------
;;; Basic lexer definitions
;;; ---------------------------------------------------------

(defclass position ()
  ((line :accessor line
         :initarg :line
         :type number)
   (column :accessor column
           :initarg :column
           :type number)))

(defclass lexem ()
  ((start-position :accessor spos
                   :initarg :start
                   :type position)
   (end-position :accessor epos
                 :initarg :end
                 :type position)))

(defclass delimiter (lexem)
  ())

(defclass whitespace (lexem)
  ())

(defclass identifier (lexem)
  ((name :accessor name
         :initarg :name
         :type string)))

(defclass literal (lexem)
  ((value :accessor value
          :initarg :value
          :type string)))


;;; ---------------------------------------------------------
;;; Lexems definitions
;;; ---------------------------------------------------------

(defclass column (delimiter)
  ())

(defclass semi-column (delimiter)
  ())

(defclass pipe (delimiter)
  ())

(defclass o-parenthesis (delimiter)
  ())

(defclass c-parenthesis (delimiter)
  ())

(defclass plus (delimiter)
  ())

(defclass star (delimiter)
  ())

(defclass interrogation (delimiter)
  ())

(defclass o-bracket (delimiter)
  ())

(defclass c-bracket (delimiter)
  ())

(defclass dot (delimiter)
  ())

(defclass arrow (delimiter)
  ())


;;; ---------------------------------------------------------
;;; Lexer utils
;;; ---------------------------------------------------------

(defun make-hash-set (lst &key test)
  (let* ((res (make-hash-table :test test)))
    (mapc (lambda (x)
            (setf (gethash x res) t)) lst)
    res))


;;; ---------------------------------------------------------
;;; lexer regexps
;;; ---------------------------------------------------------
(defun delimiter-char-p (char)
  (string-match-p "[][;:()+*|?.]" (char-to-string char)))

(defun whitespace-char-p (char)
  (string-match-p "[\s\n\t]" (char-to-string char)))

(defun literal-char-p (char)
  (string-match-p "[']" (char-to-string char)))

(defun identifier-char-p (char)
  (string-match-p "[_[:word:]]" (char-to-string char)))

(defun identifier-p (str)
  (string-match-p "[_[:word:]]*" str))

(defun multi-delimiter-p (str)
  (string-match-p "//(->//)" str))


(defun str-next (str)
  (substring str 1 (length str)))

(defun str-empty-p (str)
  (= (length str) 0))

(defun make-id (str)
  (make-instance 'identifier
                 :name str
                 :start (make-instance 'position
                                       :column *start-column*
                                       :line *cur-line*)
                 :end (make-instance 'position
                                     :column *cur-column*
                                     :line *cur-line*)))

(defun make-lit (str)
  (make-instance 'literal
                 :value str
                 :start (make-instance 'position
                                       :column *start-column*
                                       :line *cur-line*)
                 :end (make-instance 'position
                                     :column *cur-column*
                                     :line *cur-line*)))

(defun make-delim (str)
  (let ((type (cond
               ((equal str ";") 'semi-column)
               ((equal str ":") 'column)
               ((equal str "(") 'o-parenthesis)
               ((equal str ")") 'c-parenthesis)
               ((equal str "[") 'o-bracket)
               ((equal str "]") 'c-bracket)
               ((equal str "+") 'plus)
               ((equal str "*") 'star)
               ((equal str "|") 'pipe)
               ((equal str "?") 'interrogation)
               ((equal str ".") 'dot)
               (t (error "%s is not a delimiter" str)))))
    (make-instance type
                   :start (make-instance 'position
                                         :column *cur-column*
                                         :line *cur-line*)
                   :end (make-instance 'position
                                       :column (+ *cur-column* 1)
                                       :line *cur-line*))))


;;; ---------------------------------------------------------
;;; Lexer itself
;;; ---------------------------------------------------------

(defvar *cur-column* 0)
(defvar *cur-line* 0)
(defvar *start-column* 0)

(defun lexer (str)
  (unless (str-empty-p str)
    (let ((*cur-line* 1) (*cur-column* 1)
          (acc "") (lit-acc "") (delim-acc "")
          (res) (in-literal))

      (cl-labels
          ((%mk-delim ()
                      (unless (str-empty-p delim-acc)
                        (push (make-delim delim-acc) res)
                        (setf delim-acc "")))
           (%mk-ident ()
                      (unless (str-empty-p acc)
                        (push (make-id acc) res)
                        (setf acc "")))
           (%set-cur-pos (param)
                         (when (str-empty-p param)
                           (setf *start-column* *cur-column*))))
        (mapc
         (lambda (ch)
           (cond
            ((literal-char-p ch)
             (%mk-delim)
             (%mk-ident)
             (if (eq ch in-literal)
                 (progn
                   (push (make-lit lit-acc) res)
                   (setf lit-acc "")
                   (setf in-literal nil))
               (progn
                 (%set-cur-pos lit-acc)
                 (setf in-literal ch))))

            (in-literal
             (setf lit-acc (concat lit-acc (char-to-string ch))))

            ((delimiter-char-p ch)
             (%mk-ident)
             (%set-cur-pos delim-acc)
             (setf delim-acc (concat delim-acc (char-to-string ch))))

            ((whitespace-char-p ch)
             (%mk-delim)
             (%mk-ident)
             (when (eq ch ?\n)
               (setf *cur-column* 1)
               (incf *cur-line*)))

            ((identifier-char-p ch)
             (%mk-delim)
             (%set-cur-pos acc)
             (setf acc (concat acc (char-to-string ch))))
            (t (error "give up at '%s'" (char-to-string ch))))

           (incf *cur-column*))
         (string-to-list str))

        (%mk-delim)
        (%mk-ident))
      (reverse res))))


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
    (let* ((rule/rest (rule lexems))
           (rule (car rule/rest))
           (rest (cdr rule/rest)))
      (append rule (rule-list rest)))))

(defun rule (lexems)
  (when lexems
    (let* ((sort/rest (sort lexems))
           (sort (car sort/rest))
           (rest (cdr sort/rest)))
      (unless (typep (car rest) 'column)
        (error "RULE: column expected. line: %d column: %d"
               (line (spos (car rest)))
               (column (spos (car rest)))))
      (let* ((command-list/rest (command-list (cdr rest)))
             (command-list (car commad-list/rest))
             (rest (cdr command-list/rest)))
        (unless (typep (car rest) 'semi-column)
          (error "RULE: semi-column expected. line: %d column: %d"
                 (line (spos (car rest)))
                 (column (spos (car rest)))))
        (cons (make-instance 'rule-node
                             :name sort
                             :members command-list
                             :start (spos sort)
                             :end (epos (car rest)))
              (cdr rest))))))

(defun sort (lexems)
  (when lexems
    (if (typep (car lexems) 'identifier)
        (cons (make-instance 'sort-node
                             :name (name (car lexems))
                             :start (spos (car lexems))
                             :end (epos (second lexems)))
              (cdr lexems))
      (error "SORT: identifier expected. line: %d column: %d"
             (line (spos (car lexems)))
             (column (spos (car lexems)))))))

(defun command-list (lexems)
  (when lexems
    (let* ((command/rest (command lexems))
           (command (car command/rest))
           (rest (cdr command/rest)))
      (cons command (command-list-rest rest)))))

(defun command-list-rest (lexems)
  (when lexems
    (unless (typep (car lexems) 'pipe)
      (error "COMMAND-REST: pipe expected. line: %d column: %d"
             (line (spos (car lexems)))
             (column (spos (car lexems)))))
    (let* ((command/rest (command lexems))
           (command (car command/rest))
           (rest (cdr command/rest)))
      (cons command (command-list-rest rest)))))

(defun command (lexems)
  (let* ((ent-list/rest (entity-list lexems))
         (ent-list (car ent-list/rest))
         (rest (cdr ent-list/rest)))
    (cons (make-instance 'command-node
                         :members ent-list
                         :start (spos (car ent-list))
                         :ent (epos (car (last ent-list))))
          rest)))

(defun entity-list (lexems)
  (when lexems
    (let* ((entity/rest (entity lexems))
           (entity (car entity/rest))
           (rest (cdr entity/rest)))
      (if entity
          (append (list entity) (entity-list rest))
        rest))))

(defun entity (lexems)
  (when lexems
    (if (or (typep (car lexems) 'identifier)
            (typep (car lexems) 'literal))
        (cons (make-instance 'entity-node
                             :value (car lexems)
                             :start (spos (car lexems))
                             :end (epos (car lexems)))
              (cdr lexems))
      (list nil lexems))))


(defun test () (entity (lexer "Aa")))
