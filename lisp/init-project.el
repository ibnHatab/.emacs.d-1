;;; init-project.el --- Project management, VCS, search -*- lexical-binding: t; -*-
;;; Commentary:
;; Projectile, Magit, git-gutter and ag/wgrep search.  Keybindings preserved
;; from the previous config (C-c p prefix, C-x g for magit, f9 for search).
;;; Code:

;; ---------------------------------------------------------------------------
;; Projectile
;; ---------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :defer 1
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom (projectile-enable-caching t)
  :config (projectile-mode +1))

;; ---------------------------------------------------------------------------
;; Magit + git-gutter
;; ---------------------------------------------------------------------------
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-branch-arguments nil)
  (magit-push-always-verify nil))

;; diff-hl replaces git-gutter: better maintained, refreshes with magit, and
;; renders in the fringe (or margin in a terminal).
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-x C-g" . diff-hl-mode)
         ("C-x v =" . diff-hl-show-hunk)
         ("C-x p"   . diff-hl-previous-hunk)
         ("C-x n"   . diff-hl-next-hunk)
         ("C-x r"   . diff-hl-revert-hunk))
  :config (global-diff-hl-mode))

;; ---------------------------------------------------------------------------
;; Search: ag + wgrep (editable results)
;; ---------------------------------------------------------------------------
(use-package ag
  :ensure t
  :commands (ag ag-project)
  :bind ("<f9>" . ag)
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t))

(use-package wgrep
  :ensure t
  :custom (wgrep-auto-save-buffer t))

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup))

;; ---------------------------------------------------------------------------
;; Tree view (treemacs replaces the unmaintained neotree)
;; ---------------------------------------------------------------------------
(use-package treemacs
  :ensure t
  :defer t
  :bind (("s-s" . treemacs)                    ; toggle on the current root
         ("s-S" . treemacs-select-directory))  ; prompt for a new dir, then show
  :custom
  (treemacs-width 32)
  (treemacs-follow-after-init t)
  :config
  (treemacs-follow-mode 1)        ; keep the tree in sync with the open buffer
  (treemacs-filewatch-mode 1)     ; refresh on external file changes
  (treemacs-git-mode 'simple))    ; show git status in the tree

;; ---------------------------------------------------------------------------
;; TRAMP (remote editing)
;; ---------------------------------------------------------------------------
(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-default-method "ssh")
  ;; Use bash on the remote without the old global (setenv "SHELL" ...) hack,
  ;; which changed the shell for all of Emacs rather than just TRAMP.
  (tramp-default-remote-shell "/bin/bash")
  :config
  ;; Honour the remote user's PATH so remote programs are found.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(provide 'init-project)
;;; init-project.el ends here
