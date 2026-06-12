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

(use-package git-gutter
  :ensure t
  :defer t
  :bind (("C-x C-g" . git-gutter:toggle)
         ("C-x v =" . git-gutter:popup-hunk)
         ("C-x p"   . git-gutter:previous-hunk)
         ("C-x n"   . git-gutter:next-hunk)
         ("C-x r"   . git-gutter:revert-hunk)))

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
;; Tree view
;; ---------------------------------------------------------------------------
(use-package neotree
  :ensure t
  :defer t
  :bind ("s-s" . neotree-toggle))

;; ---------------------------------------------------------------------------
;; TRAMP (remote editing)
;; ---------------------------------------------------------------------------
(use-package tramp
  :ensure nil
  :defer t
  :custom (tramp-default-method "ssh")
  :config (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(provide 'init-project)
;;; init-project.el ends here
