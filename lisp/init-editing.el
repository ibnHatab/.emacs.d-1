;;; init-editing.el --- Text editing enhancements -*- lexical-binding: t; -*-
;;; Commentary:
;; Multiple cursors, expand-region, snippets, jumping, undo history,
;; bookmarks (bm) and symbol highlighting.  Preserves the original keybindings.
;;; Code:

;; ---------------------------------------------------------------------------
;; Multiple cursors
;; ---------------------------------------------------------------------------
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;; ---------------------------------------------------------------------------
;; Expand region
;; ---------------------------------------------------------------------------
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; ---------------------------------------------------------------------------
;; Snippets
;; ---------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :defer 1
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)
  (setq yas-prompt-functions '(yas-completing-prompt)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; ---------------------------------------------------------------------------
;; Jumping around
;; ---------------------------------------------------------------------------
(use-package ace-jump-mode
  :ensure t
  :bind ("C-\\" . ace-jump-mode))

;; ---------------------------------------------------------------------------
;; Undo history (visual + persistent)
;; ---------------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory)))))

;; ---------------------------------------------------------------------------
;; Commenting
;; ---------------------------------------------------------------------------
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; ---------------------------------------------------------------------------
;; Symbol highlighting (f3 family)
;; ---------------------------------------------------------------------------
(use-package highlight-symbol
  :ensure t
  :diminish ""
  :custom (highlight-symbol-idle-delay 1.5)
  :bind (([(control f3)] . highlight-symbol)
         ([f3]           . highlight-symbol-next)
         ([(shift f3)]   . highlight-symbol-prev)
         ([(meta f3)]    . highlight-symbol-query-replace)))

;; ---------------------------------------------------------------------------
;; Visual bookmarks (f2 family)
;; ---------------------------------------------------------------------------
(use-package bm
  :ensure t
  :demand t
  :init (setq bm-restore-repository-on-load t)
  :custom
  (bm-cycle-all-buffers t)
  (bm-repository-file (expand-file-name "bm-repository" user-emacs-directory))
  (bm-buffer-persistence t)
  :config
  (add-hook 'after-init-hook #'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook (lambda () (bm-buffer-save-all) (bm-repository-save)))
  :bind (("<f2>"   . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle)
         ("<left-fringe> <mouse-1>" . bm-toggle-mouse)))

;; ---------------------------------------------------------------------------
;; Misc editing modes
;; ---------------------------------------------------------------------------
(use-package which-key :ensure t :diminish "" :config (which-key-mode))
(use-package popwin    :ensure t :config (popwin-mode 1))
(use-package writegood-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package protobuf-mode :ensure t :defer t)

;; ---------------------------------------------------------------------------
;; Markdown + Mermaid diagram preview
;; ---------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; Make sure the nvm-installed `mmdc' CLI is on exec-path even when Emacs is
;; launched from a desktop session that doesn't source the nvm shell init.
(let ((nvm-bin (car (last (file-expand-wildcards
                           (expand-file-name "~/.nvm/versions/node/*/bin"))))))
  (when (and nvm-bin (file-directory-p nvm-bin))
    (add-to-list 'exec-path nvm-bin)
    (setenv "PATH" (concat nvm-bin path-separator (getenv "PATH")))))

;; markdown-mermaid.el: render ```mermaid``` blocks via the mmdc CLI.
;; Pinned git checkout under site-lisp/ (no MELPA/`:vc` needed on Emacs 28).
(let ((dir (expand-file-name "site-lisp/markdown-mermaid" user-emacs-directory)))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(use-package markdown-mermaid
  :ensure nil
  :after markdown-mode
  :commands (markdown-mermaid-preview)
  :bind (:map markdown-mode-map
              ("C-c m" . markdown-mermaid-preview))
  :config
  ;; Resolve mmdc now that nvm's bin is on exec-path.
  (setq markdown-mermaid-mmdc-path (or (executable-find "mmdc")
                                       markdown-mermaid-mmdc-path)))

(provide 'init-editing)
;;; init-editing.el ends here
