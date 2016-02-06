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

(defvar *delimiters* (make-hash-set '(?\; ?\: ?\( ?\) ?\[ ?\] ?\+ ?\* ?\| ?\? ?\.) :test #'eq))

(defun delimiter-p (char)
  (gethash char *delimiters*))

(defvar *whitespaces* (make-hash-set '(?\s ?\n ?\t) :test #'eq))

(defun whitespace-p (char)
  (gethash char *whitespaces*))

(defvar *literal-chars* (make-hash-set '(?\') :test #'eq))

(defun literal-char-p (char)
  (gethash char *literal-chars*))

(defun identifier-p (char)
  (or (eq char ?_)
      (string-match-p "[[:word:]]" (char-to-string char))))

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
               ((eq ch ?.) 'dot)
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

(defun lexer (str)
  (unless (str-empty-p str)
    (let ((*cur-line* 1) (*cur-column* 1)
          (acc "") (res) (in-literal))

      (mapc
       (lambda (ch)
         (cond
          ((literal-char-p ch)
           (if (eq ch in-literal)
               (progn
                 (push (make-lit acc) res)
                 (setf acc "")
                 (setf in-literal nil))
             (progn
               (when (str-empty-p acc)
                 (setf *start-column* *cur-column*))
               (setf in-literal ch))))
          (in-literal
           (setf acc (concat acc (char-to-string ch))))

          ((delimiter-p ch)
           (unless (str-empty-p acc)
             (push (make-id acc) res)
             (setf acc ""))
           (push (make-delim ch) res))

          ((whitespace-p ch)
           (unless (str-empty-p acc)
             (push (make-id acc) res)
             (setf acc ""))
           (when (eq ch ?\n)
             (setf *cur-column* 1)
             (incf *cur-line*)))

          ((identifier-p ch)
           (when (str-empty-p acc)
             (setf *start-column* *cur-column*))
           (setf acc (concat acc (char-to-string ch))))
          (t (error "give up at '%s'" (char-to-string ch))))

         (incf *cur-column*))
       (string-to-list str))

      (unless (str-empty-p acc)
        (push (make-id acc) res)
        (setf acc ""))
      (reverse res))))
