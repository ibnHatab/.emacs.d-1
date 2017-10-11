;;; init.el --- My emacs config file

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable splash screen
(setq inhibit-startup-screen t)

;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; add lisp directry to path (recursive)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
	  (dir (nth 1 file)))
      (when (and dir
		 (not (string-suffix-p "." filename)))
	(add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/bin")
(add-to-list 'exec-path "/usr/local/bin")

;; Take out the trash
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package s
  :ensure t
  :defer 1)

(use-package dash :ensure t)

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (defadvice wgrep-change-to-wgrep-mode (after wgrep-set-normal-state)
    (if (fboundp 'evil-normal-state)
	(evil-normal-state)))
  (ad-activate 'wgrep-change-to-wgrep-mode)

  (defadvice wgrep-finish-edit (after wgrep-set-motion-state)
    (if (fboundp 'evil-motion-state)
	(evil-motion-state)))
  (ad-activate 'wgrep-finish-edit))

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup))

(use-package ag
  :ensure t
  :commands (ag ag-project)
  :config
  (add-hook 'ag-mode-hook
	    (lambda ()
	      (wgrep-ag-setup)
	      (define-key ag-mode-map (kbd "n") 'evil-search-next)
	      (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
  (setq ag-executable "/usr/bin/ag")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))
(use-package evil-commentary
  :ensure t)
(use-package powerline
  :ensure t)
(use-package evil
  :ensure t)
(use-package powerline-evil
  :ensure t)
(use-package comment-dwim-2
  :ensure t)
(use-package undo-tree
  :ensure t
  :diminish t
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
	(list (cons "." (expand-file-name "undo-tree-history" user-emacs-directory)))))

(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(use-package markdown-mode
	     :ensure t)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(require 'powerline)
(require 'powerline-evil)
(powerline-default-theme)

;; load theme
(load-theme 'zenburn t)

(require 'init-global)
(require 'init-keybindings)
(require 'init-evil)
(require 'init-flycheck)



(use-package flycheck
  :ensure t
  :commands flycheck-mode)

;;; Flycheck mode
(add-hook 'flycheck-mode-hook
	  (lambda ()
	    (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
	    (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

(provide 'init)
;;; init.el ends here
