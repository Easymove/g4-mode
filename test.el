(defun test ()
  (parser
   (lexer "sort1 : a b 'c' | d e f ;
sort2 : fuck | fucking 'fuck' ;
fucking : 'fucking' ;")))
