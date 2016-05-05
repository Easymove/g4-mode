(defvar g4-mode-hook nil)
(defvar g4-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-j") 'newline-and-indent)
    (define-key map (kbd "M-.") 'g4-goto-definition)
    (define-key map (kbd "M-,") 'g4-restore-position)
    (define-key map (kbd "C-h u") 'g4-highlight-unused)
    (define-key map (kbd "C-h d") 'g4-highlight-undefined)
    (define-key map (kbd "C-h z") 'g4-unhighlight-all)
    map))
(defvar *current-grammar* nil)

(defconst g4-font-lock-keywords
  (list
   '("\\<EOF\\|skip\\>" . font-lock-builtin-face)
   '("\\(^\\w*\\)\\>" . font-lock-type-face)
   '("//.*" . font-lock-comment-face)
   '("\\('.*?'\\)\\|\\(\\\\.\\)" . font-lock-string-face)
   '("[:|;]" . font-lock-keyword-face)))


(defun g4-indent-line ()
  "indent current line in g4 mode"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0))

  (let (cur-indent)
    (if (or (looking-at "^[ \t]*;")
            (looking-at "^[ \t]*:")
            (looking-at "^[ \t]*|"))
        (setq cur-indent 4)
      (save-excursion
        (forward-line -1)
        (if (looking-at "^[ \t]*\\w*[ \t]*:.*")
            (setq cur-indent 6))))

    (if cur-indent
        (indent-line-to cur-indent)
      (indent-line-to 0))))

(defvar g4-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)
    st))

(defun load-g4 ()
  (load "g4-lexer.el")
  (load "g4-parser.el")
  (load "g4-utils.el")
  (load "g4-gotodef.el")
  (load "g4-analyzes.el")
  (load "g4-test.el"))

(defun parse-g4 ()
  (interactive)
  (setf *current-grammar* (parser (lexer (buffer-string))))
  (message "parsed."))

(defun g4-mode ()
  "major mode-for editing .g4 grammars"
  (interactive)
  (kill-all-local-variables)
  (load-g4)

  (set-syntax-table g4-mode-syntax-table)
  (use-local-map g4-mode-map)

  (set (make-local-variable 'font-lock-defaults) '(g4-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'g4-indent-line)

  (setq major-mode 'g4-mode)
  (setq mode-name "g4-mode")
  (run-hooks 'g4-mode-hook)

  (parse-g4))


(add-to-list 'auto-mode-alist '("\\.g4\\'" . g4-mode))

(provide 'g4-mode)
