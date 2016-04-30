;;; ------------------------------------------
;;; Test utils
;;; ------------------------------------------

(setq debug-on-error t)

(defvar *g4-unit-tests* nil)

(defmacro def-test (name params &rest body)
  (pushnew name *g4-unit-tests*)
  (list 'defun name params
        (cons 'progn body)))

(defun run-unit-tests ()
  (interactive)
  (let ((failed-tests))
    (mapc (lambda (test)
            (condition-case nil
                (funcall test)
              (error (push test failed-tests))))
          *g4-unit-tests*)
    (format "Failed tests:\n")
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
