;;; ---------------------------------------------------------
;;; Basic lexer definitions
;;; ---------------------------------------------------------

(defclass position ()
  ((line :accessor line
         :initarg :line)
   (column :accessor column
           :initarg :column)))

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
          :type (or number string))))


;;; ---------------------------------------------------------
;;; Lexems definitions
;;; ---------------------------------------------------------

(defclass column (delimiter)
  ())

(defclass single-quote (delimiter)
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

(defvar *delimiters* (make-hash-set '(?\; ?: ?( ?) ?[ ?] ?+ ?* ?| ?? ?\') :test #'eq))

(defun delimiter-p (char)
  (gethash char *delimiters*))

(defvar *whitespaces* (make-hash-set '(?\s ?\n ?\t) :test #'equal))

(defun whitespace-p (char)
  (gethash char *whitespaces*))

(defun identifier-p (char)
  (string-match-p "[[:word:]]" (char-to-string char)))

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

(defun make-delim (ch)
  (let ((type (cond
               ((eq ch ?\;) 'semi-column)
               ((eq ch ?:) 'column)
               ((eq ch ?\() 'o-parenthesis)
               ((eq ch ?\)) 'c-parenthesis)
               ((eq ch ?\[) 'o-bracket)
               ((eq ch ?\]) 'c-bracket)
               ((eq ch ?+) 'plus)
               ((eq ch ?*) 'star)
               ((eq ch ?|) 'pipe)
               ((eq ch ??) 'interrogation)
               ((eq ch ?\') 'single-quote)
               (t (error "%s is not a delimiter" (char-to-string ch))))))
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

(defun lexer-internal (str acc res)
  (if (not (str-empty-p str))
      (let* ((ch (string-to-char str)))
        (incf *cur-column*)
        (cond
         ((whitespace-p ch)
          (when (eq ch ?\n)
            (setf *cur-column* 0)
            (incf *cur-line*))
          (if (str-empty-p acc)
              (lexer-internal (str-next str) acc res)
            (lexer-internal (str-next str) "" (push (make-id acc) res))))
         ((delimiter-p ch)
          (if (str-empty-p acc)
              (lexer-internal (str-next str) acc (push (make-delim ch) res))
            (lexer-internal (str-next str) "" (append (list (make-delim ch))
                                                      (list (make-id acc))
                                                      res))))
         ((identifier-p ch)
          (when (str-empty-p acc)
            (setf *start-column* *cur-column*))
          (lexer-internal (str-next str) (concat acc (char-to-string ch)) res))
         (t (error "give up"))))
    (reverse res)))

(defun lexer (str)
  (let ((*cur-line* 1)
        (*cur-column* 0))
    (lexer-internal str "" '())))
