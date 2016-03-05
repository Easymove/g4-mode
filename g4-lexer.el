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

(defclass colon (delimiter)
  ())

(defclass semi-colon (delimiter)
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
               ((equal str ";") 'semi-colon)
               ((equal str ":") 'colon)
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

