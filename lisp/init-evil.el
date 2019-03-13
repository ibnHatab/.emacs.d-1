;;; init-evil.el --- Custom evil mode configuration.
;;; Commentary: for flycheck purposes
;;; Code: below

(defun g--config-evil-leader ()
  "Configure evil leader mode"
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "," 'other-window
    "." 'mode-line-other-buffer        ;
    ":" 'eval-expression
    "b" 'helm-mini		       ;; Switch to another buffer
    "d"	'elpy-goto-definition
    "k"	'elpy-doc
    "B"	'magit-blame-toggle
    "c"	'comment-dwim-2
    "f"	'helm-imenu
    "g"	'magit-status
    "l"	'whitespace-mode	       ;; Show invisible characters
    "o"	'delete-other-windows	       ;; C-w o
    "p"	'helm-show-kill-ring
    "s"	'ag-project		       ;; Ag search from project's root
    "S"	'delete-trailing-whitespace
    "t"	'gtags-reindex
    "T"	'xref-find-definitions
    "r" 'xref-find-references
    "#" 'server-edit
    "w"	'save-buffer
    "<SPC>"	'helm-M-x
    "y"	'simpleclip-copy)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
        (call-interactively 'magit-blame))))

(defun g--config-evil ()
  "Configure evil mode"

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
                  custom-mode
                  custom-new-theme-mode
                  dired-mode
                  eshell-mode
                  flycheck-error-list-mode
                  git-rebase-mode
                  octopress-mode
                  octopress-server-mode
                  octopress-process-mode
                  org-capture-mode
                  sunshine-mode
                  bm-show-mode
                  nov-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Use insert state in these modes
  (dolist (mode '(magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")		'evil-search-forward
    (kbd "n")		'evil-search-next
    (kbd "N")		'evil-search-previous
    (kbd "C-d")	'evil-scroll-down
    (kbd "C-u")	'evil-scroll-up
    (kbd "C-w C-w")	'other-window)

  ;; Global bindings
  (evil-define-key 'normal global-map (kbd "C-p")	'helm-projectile)
  (evil-define-key 'normal global-map (kbd "C-S-p")	'helm-projectile-switch-project)
  (evil-define-key 'normal global-map (kbd "C-`")	(lambda ()
                                                      (interactive)
                                                      (dired (expand-file-name "~"))))

  ;; Try to quit everything with escape
  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :config
  (add-hook 'evil-mode-hook 'g--config-evil)
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (g--config-evil-leader))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t))

(provide 'init-evil)
;;; init-evil.el ends here
