;;; init-cpp.el --- C/C++ settings -*- lexical-binding: t; -*-
;;; Commentary:
;; C/C++ IDE support built around clangd + compile_commands.json (compdb).
;; lsp-mode feeds completion into the corfu/cape stack (see init-completion.el).
;;
;; lsp-mode/lsp-ui latest releases require Emacs >= 29.1; we run 28.1, so the
;; last 28-compatible tags are pinned under site-lisp/.  Their package
;; dependencies still come from package.el.
;;; Code:

(let ((site (expand-file-name "site-lisp" user-emacs-directory)))
  (dolist (d '("lsp-mode" "lsp-mode/clients" "lsp-ui"))
    (let ((dir (expand-file-name d site)))
      (when (file-directory-p dir) (add-to-list 'load-path dir)))))

(dolist (dep '(dash f ht spinner markdown-mode lv))
  (unless (package-installed-p dep)
    (ignore-errors (package-install dep))))

;; lsp-mode: clangd discovers compile_commands.json by walking up from the
;; source file, so no compdb path config is needed here.
(use-package lsp-mode
  :ensure nil
  :hook ((c-mode c++-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-ui-doc-enable nil)
  ;; Let clangd insert headers itself; -j and background indexing keep
  ;; completion responsive on large compdb projects.
  (lsp-clients-clangd-args '("--header-insertion=iwyu"
                             "--background-index"
                             "-j=4"
                             "--header-insertion-decorators=0")))

(use-package lsp-ui
  :ensure nil
  :commands lsp-ui-mode)

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :ensure t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package clang-format
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

(defun my-c++-mode-hook ()
  (c-toggle-auto-hungry-state 1)
  (fset 'c-indent-region 'clang-format-region)
  (auto-fill-mode))

(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer nil t))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'clang-format-buffer-smart-on-save)

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

;; c++-mode-map keys (set once cc-mode is loaded so the map exists).
(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "C-c C-h") #'ff-find-other-file))

(provide 'init-cpp)
;;; init-cpp.el ends here
