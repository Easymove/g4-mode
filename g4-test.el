;;; ------------------------------------------
;;; Test utils
;;; ------------------------------------------

(setq debug-on-error t)

(defvar *g4-unit-tests* nil)

(defmacro def-test (name params &rest body)
  (pushnew name *g4-unit-tests*)
  (list 'defun name params
        (cons 'progn body)))

(defun g4-unit-tests ()
  (interactive)
  (let ((failed-tests))
    (mapc (lambda (test)
            (condition-case nil
                (funcall test)
              (error (push test failed-tests))))
          *g4-unit-tests*)
    (print "Failed tests:")
    (mapc (lambda (test)
            (print (symbol-name test)))
          failed-tests)
    (if failed-tests
        (message "something went wrong")
      (message "all tests pass"))))


;;; ------------------------------------------
;;; Tests
;;; ------------------------------------------

(def-test test-comment ()
  (parser
   (lexer
    "
//asd asdasd

sort1 : a b 'c' | d e f ; /**/

/* asdasdas */

sort2 : fuck | fucking 'fuck' ;

fucking : 'fucking' ;")))


(def-test test-complex-entity ()
  (parser
   (lexer
    "
sort1 : a b 'c' | d e f ; /**/

sort2 : fuck | (fucking 'fuck')+ ;

fucking : 'fucking' ;")))


(def-test test-traverse ()
  (let ((tree
         (parser
          (lexer
           "
sort1 : a b 'c' | d e f ; /**/

sort2 : fuck | (fucking 'fuck')+ ;

fucking : 'fucking' ;"))))

    (traverse tree
              (lambda (node)
                (print node)))))


(def-test test-find-def ()
  (let ((*current-grammar*
         (parser
          (lexer
           "
sort1 : a b 'c' | d e f ; /**/

sort2 : fuck | (fucking 'fuck')+ ;

fucking : 'fucking' ;"))))

    (find-def "sort1")))


(def-test test-first-set ()
  (let ((*current-grammar*
         (parser
          (lexer
           "
A : B C | 'a' E ;

B : 'b' | ;

C : F | 'c' ;

F : 'f1' | 'f2' ;

E : B | 'e' ;"))))

    (format "FIRST(A): %s" (mapcar #'node-string-name (first-set (lookup-name "A"))))))


(def-test test-follow-set ()
  (let ((*current-grammar*
         (parser
          (lexer
           "
A : B C | 'a' E ;

B : 'b' | ;

C : F | 'c' ;

F : 'f1' | 'f2' ;

E : B E | 'e' ;"))))

    (format "FOLLOW(B): %s" (mapcar #'node-string-name (follow-set (lookup-name "B"))))))
