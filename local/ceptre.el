;; Clear syntax highlighting
(setq-default font-lock-defaults nil)

;; Just about anything in Ceptre is a keyword character
(modify-syntax-entry ?\" "w")
(modify-syntax-entry ?~ "w")
(modify-syntax-entry ?@ "w")
(modify-syntax-entry ?! "w")
(modify-syntax-entry ?# "w")
(modify-syntax-entry ?' "w")
(modify-syntax-entry ?- ". 45b")
(modify-syntax-entry ?* ". 45b")
(modify-syntax-entry ?/ ". 47b")
(modify-syntax-entry ?0 ". 57b")
(modify-syntax-entry ?; ". 59b")
(modify-syntax-entry ?^ ". 94b")
(modify-syntax-entry ?| ". 124b")

;; Define syntax highlighting
(font-lock-add-keywords
 'ceptr-mode
 '(("\\<\\(mode\\|interactive\\|trace\\|builtin\\)\\>" . 'font-lock-keyword-face)
   ("\\<\\(type\\|pred\\|bwd\\|stage\\|context\\)\\>" . 'font-lock-keyword-face)
   ("\\(:\\|\\.\\|=\\)" . 'font-lock-builtin-face)
   ("\\<\\([A-Z_]\\w*\\)\\>" . 'font-lock-constant-face)
   ("\\(->\\|<-\\|-o\\|o-\\|*\\)" . 'font-lock-type-face)
   ("\\(^\\s-*[^()A-Z_][^()]*\\s-*:\\)" . 'font-lock-function-name-face)
   ("{\\|}" . 'font-lock-type-face)
   ("\\[\\|\\]" . 'font-lock-type-face)
   ("[^A-Z_{\\[]\\w*\\s-*:" . 'font-lock-function-name-face)
   ("{" . 'font-lock-type-face)
   ("}" . 'font-lock-type-face)
   ("(" . 'font-lock-type-face)
   (")" . 'font-lock-type-face)
   ("%\\(.*\\)" . 'font-lock-comment-face)
   ("%{\\(.\\|\n\\)*?%}" . 'font-lock-comment-face)))

;; Set comment syntax
(setq-default comment-start "%")
(setq-default comment-end "")

;; Assign colors
(custom-set-faces
 '(font-lock-type-face ((t (:foreground "cyan"))))
 '(font-lock-keyword-face ((t (:foreground "magenta"))))
 '(font-lock-builtin-face ((t (:foreground "yellow"))))
 '(font-lock-constant-face ((t (:foreground "green"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-comment-face ((t (:foreground "gray")))))

;; Set indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default lisp-indent-offset 4)

;; Set the current syntax name
(setq b:current_syntax "ceptre")
