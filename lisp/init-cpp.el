;;; init-cpp.el -- c++ settings
;;;
;;; Code: down below
;;;
;;; Commentary:

(use-package lsp-mode
  :commands lsp
  :ensure t)
(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  )
;(use-package company-lsp
;  :ensure t
;  :commands company-lsp
;  :config
;  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;  (push 'company-lsp company-backends)) ;; add company-lsp as a backend

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :ensure t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

;(use-package xah-lookup
;  :ensure t
;  :init
;)

(use-package clang-format
  :ensure t
  :init
)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

;; cat *.cpp > single.cpp
;;  M-x c-guess-no-install and then M-x c-guess-view
(c-add-style "hwp-c-style"
			 '("gnu"
			   (c-basic-offset . 4)		; Guessed value
			   (c-offsets-alist
				(block-close . 0)		; Guessed value
				(case-label . +)		; Guessed value
				(defun-block-intro . +)	; Guessed value
				(defun-close . 0)		; Guessed value
				(defun-open . 0)		; Guessed value
		 		(else-clause . 0)		; Guessed value
				(innamespace . 0)		; Guessed value
				(namespace-close . 0)		; Guessed value
				(statement . 0)				; Guessed value
				(statement-block-intro . +) ; Guessed value
				(statement-case-intro . +) ; Guessed value
				(substatement . +)		; Guessed value
				(topmost-intro . 0)		; Guessed value
				(access-label . -)
				(annotation-top-cont . 0)
				(annotation-var-cont . +)
				(arglist-close . c-lineup-close-paren)
				(arglist-cont c-lineup-gcc-asm-reg 0)
				(arglist-cont-nonempty . c-lineup-arglist)
				(arglist-intro . c-lineup-arglist-intro-after-paren)
				(block-open . 0)
				(brace-entry-open . 0)
				(brace-list-close . 0)
				(brace-list-entry . c-lineup-under-anchor)
				(brace-list-intro . c-lineup-arglist-intro-after-paren)
				(brace-list-open . +)
				(c . c-lineup-C-comments)
				(catch-clause . 0)
				(class-close . 0)
				(class-open . 0)
				(comment-intro . c-lineup-comment)
				(composition-close . 0)
				(composition-open . 0)
				(cpp-define-intro c-lineup-cpp-define +)
				(cpp-macro . -1000)
				(cpp-macro-cont . +)
				(do-while-closure . 0)
				(extern-lang-close . 0)
				(extern-lang-open . 0)
				(friend . 0)
				(func-decl-cont . +)
				(inclass . +)
				(incomposition . +)
				(inexpr-class . +)
				(inexpr-statement . +)
				(inextern-lang . +)
				(inher-cont . c-lineup-multi-inher)
				(inher-intro . +)
				(inlambda . c-lineup-inexpr-block)
				(inline-close . 0)
				(inline-open . 0)
				(inmodule . +)
				(knr-argdecl . 0)
				(knr-argdecl-intro . 5)
				(label . 0)
				(lambda-intro-cont . +)
				(member-init-cont . c-lineup-multi-inher)
				(member-init-intro . +)
				(module-close . 0)
				(module-open . 0)
				(namespace-open . 0)
				(objc-method-args-cont . c-lineup-ObjC-method-args)
				(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
				(objc-method-intro .
								   [0])
				(statement-case-open . +)
				(statement-cont . +)
				(stream-op . c-lineup-streamop)
				(string . -1000)
				(substatement-label . 0)
				(substatement-open . +)
				(template-args-cont c-lineup-template-args +)
				(topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont))))

(defun my-c++-mode-hook ()
  (c-set-style "hwp-c-style")
  (c-toggle-auto-hungry-state 1)
  (fset 'c-indent-region 'clang-format-region)
  (auto-fill-mode))

(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer nil t))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'clang-format-buffer-smart-on-save)

(require 'doxymacs)
(add-hook 'c++-mode-hook 'doxymacs-mode)
;;
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
	  (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)



;; autoinsert C/C++ header
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
  '(nil
	"///\n"
	"/// @file " (file-name-nondirectory buffer-file-name) "\n"
	"/// @author <smp28rd> \n"
	"///\n"
	"/// @brief\n"
	"///\n"
	"///\n"
	"///\n"
	(make-string 70 ?/) "\n\n"
	(let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
		   (nopath (file-name-nondirectory noext))
		   (ident (concat "__" (upcase nopath) "_H__")))
	  (concat "#ifndef " ident "\n"
			  "#define " ident  "\n\n\n"
			  "\n\n#endif // " ident "\n"))
	))

;; auto insert C/C++
(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++ implementation")
  '(nil
	"///\n"
	"/// @file " (file-name-nondirectory buffer-file-name) "\n"
	"/// @author <smp28rd> \n"
	"///\n"
	"/// @brief\n"
	"///\n"
	"///\n"
	(make-string 70 ?/) "\n\n"
	(let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
		   (nopath (file-name-nondirectory noext))
		   (ident (concat nopath ".h")))
	  (if (file-exists-p ident)
		  (concat "#include \"" ident "\"\n")))
	(make-string 70 ?/) "\n"
	))


(defun xah-lookup-cppreference (&optional word)
  "Lookup definition of current word or text selection in URL."
  (interactive)
  (xah-lookup-word-on-internet
   word
   ;; Use word02051 as a placeholder in the query URL.
   "http://en.cppreference.com/mwiki/index.php?search=word02051"
   xah-lookup-browser-function))


;; Add shortcut for c++-mode
(define-key c++-mode-map (kbd "C-c d c") #'xah-lookup-cppreference)

;; Another example with http://www.boost.org
(defun xah-lookup-boost (&optional word)
  (interactive)
  (xah-lookup-word-on-internet
   word
   "https://cse.google.com/cse?cx=011577717147771266991:jigzgqluebe&q=word02051"
   xah-lookup-browser-function))
(define-key c++-mode-map (kbd "C-c d b") #'xah-lookup-boost)
(define-key c++-mode-map (kbd "C-c C-h") #'ff-find-other-file)


(provide 'init-cpp)
;;; init-cpp.el ends here
