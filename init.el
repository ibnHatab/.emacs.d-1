
(add-to-list
 'load-path
 (expand-file-name "local" user-emacs-directory))

(setq backup-directory-alist `(("." . "~/.saves")))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Turn off bell alarms
(setq ring-bell-function 'ignore)

;; Disable splash screen
(setq inhibit-startup-screen t)

(package-initialize)
;; package.el
(require 'package)
(setq package-enable-at-startup nil)

;; add lisp directry to path (recursive)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
	  (dir (nth 1 file)))
      (when (and dir
		 (not (string-suffix-p "." filename)))
	(add-to-list 'load-path (car file))))))

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/bin")


;; Take out the trash
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-utils)
(require 'init-elpa)

(require 'google-this)
(google-this-mode 1)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t
  :defer t)

(use-package protobuf-mode
  :ensure t
  :defer t)

(use-package s
  :ensure t
  :defer 1)

(use-package dash :ensure t)

(use-package expand-region
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-mini-default-sources
      '(helm-source-buffers-list
        helm-source-bookmarks
        helm-source-recentf
        helm-source-buffer-not-found))
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-limit 100)
  (setq-default
   helm-recentf-fuzzy-match 't
   helm-buffers-fuzzy-match 't
   helm-locate-fuzzy-match 't
   helm-M-x-fuzzy-match 't
   helm-imenu-fuzzy-match 't
   helm-apropos-fuzzy-match 't
   helm-lisp-completion-at-point 't
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   )
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))


(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)

  (defun org-keyword-backend (command &optional arg &rest ignored)
    "Company backend for org keywords.
COMMAND, ARG, IGNORED are the arguments required by the variable
`company-backends', which see."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'org-keyword-backend))
      (prefix (and (eq major-mode 'org-mode)
                   (let ((p (company-grab-line "^#\\+\\(\\w*\\)" 1)))
                     (if p (cons p t)))))
      (candidates (mapcar #'upcase
                          (cl-remove-if-not
                           (lambda (c) (string-prefix-p arg c))
                           (pcomplete-completions))))
      (ignore-case t)
      (duplicates t)))
  (add-to-list 'company-backends 'org-keyword-backend)

  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (global-set-key (kbd "<C-tab>") 'company-manual-begin)
  (define-key company-active-map (kbd "ESC") 'company-abort)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package counsel :ensure t)

(use-package dictionary :ensure t)

(use-package gist
  :ensure t)

(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project)
  :ensure t)

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup))

(use-package ag
  :ensure t
  :commands (ag ag-project)
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

(use-package comment-dwim-2
  :ensure t)

(use-package color-theme-sanityinc-solarized :ensure t)

;;(use-package mmm-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-\\" . ace-jump-mode)))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode t)
  ;; (define-key yas-minor-mode-map (kbd "<tab>") #'yas-expand)
  (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (yas-reload-all)
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt)))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :diminish ""
  :config
  (which-key-mode t))

(use-package projectile
  :ensure t
  :defer 1
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t))

(use-package highlight-symbol
  :ensure t
  ;; :defer nil
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.5)
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil))

(use-package git-gutter
  :ensure t
  :defer t)

;; (use-package undo-tree
;;   :ensure t
;;   :diminish undo-tree-mode
;;   :config
;;   (setq undo-tree-auto-save-history t)
;;   (setq undo-tree-history-directory-alist
;; 	(list (cons "." (expand-file-name "undo-tree-history" user-emacs-directory)))))

(use-package writegood-mode
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (writegood-mode 0))))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package bm
  :ensure t
  :demand t
  :init
  (setq bm-restore-repository-on-load t)
  :config
  (setq bm-cycle-all-buffers t)
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  (setq-default bm-buffer-persistence t)
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("<left-fringe> <mouse-5>" . bm-next-mouse)
         ("<left-fringe> <mouse-4>" . bm-previous-mouse)
         ("<left-fringe> <mouse-1>" . bm-toggle-mouse)
         ("C-<f2>" . bm-toggle))
  )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package markdown-mode
	     :ensure t)

(require 'init-platform)
;;(require 'init-global)
(require 'init-keybindings)
(require 'init-fonts)
(require 'init-flycheck)
(require 'init-git)
(require 'init-org)
(require 'init-go)
(require 'init-python)
(require 'init-cpp)
(require 'init-julia)

(use-package lsp-mode
  :hook (prog-mode . lsp))

(use-package lsp-ui
  :ensure t)
(use-package company-lsp
  :ensure t)
(use-package neotree
  :ensure t
  :defer t
)
(use-package whole-line-or-region
  :ensure t)

(ad-activate 'term-sentinel)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq scroll-step 1)                    ; scrolling page
(setq-default tab-width 4)

;; IDO
(ido-mode t)
(setq ido-enable-flex-matching t)
(fset 'yes-or-no-p 'y-or-n-p)

(defun my-turn-modes (param &rest modes)
  (mapcar #'(lambda (mode)
              (funcall mode param)) modes))

(my-turn-modes 1
               'global-auto-revert-mode
               'global-company-mode
               'global-hl-line-mode
               'which-key-mode
               'winner-mode
			   'delete-selection-mode
               'whole-line-or-region-mode)

(setq company-global-modes '(not org-mode go-mode js2-mode cmake-mode shell-mode))
(setq-default which-key-idle-delay 0.9)

(load-theme 'sanityinc-solarized-dark t)

(setenv "EDITOR" "emacsclient")

(server-start)
(provide 'init)
;;; init.el ends here
