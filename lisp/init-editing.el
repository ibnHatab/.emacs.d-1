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
;; avy replaces the unmaintained ace-jump-mode (which pulled in the deprecated
;; `cl' compatibility library).  C-\ preserved; type a few chars then a hint.
(use-package avy
  :ensure t
  :bind (("C-\\"  . avy-goto-char-timer)
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)))

;; ---------------------------------------------------------------------------
;; Undo history: vundo (lightweight visual tree over Emacs' built-in undo)
;; replaces undo-tree, which was heavy and prone to corrupting its history.
;; ---------------------------------------------------------------------------
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))
;; Persist built-in undo across sessions (the feature undo-tree gave us).
(use-package undohist
  :ensure t
  :init (undohist-initialize)
  :custom
  (undohist-directory (expand-file-name "undohist" user-emacs-directory)))

;; ---------------------------------------------------------------------------
;; Commenting
;; ---------------------------------------------------------------------------
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; ---------------------------------------------------------------------------
;; VSCode-style buffer cycling: C-Tab walks the most-recently-used buffer
;; stack (shown as a horizontal list in the echo area); C-S-Tab goes back.
;; The order isn't committed until you stop flipping, so repeated C-Tab keeps
;; walking further back in time -- the "hold Ctrl, tap Tab" feel.
;; ---------------------------------------------------------------------------
(use-package iflipb
  :ensure t
  :bind (("C-<tab>"   . iflipb-next-buffer)
         ("C-S-<tab>" . iflipb-previous-buffer)
         ("C-S-<iso-lefttab>" . iflipb-previous-buffer)) ; X11 alt for C-S-Tab
  :custom
  (iflipb-wrap-around t)                 ; cycle past the ends
  (iflipb-ignore-buffers '("^ "          ; internal/space-prefixed buffers
                           "^\\*helm" "^\\*Echo" "^\\*Minibuf"
                           "^\\*Completions" "^\\*scratch")))

;; ---------------------------------------------------------------------------
;; Symbol highlighting (f3 family)
;; ---------------------------------------------------------------------------
;; symbol-overlay replaces the old (unmaintained) highlight-symbol package.
;; symbol-overlay-mode auto-highlights the symbol at point after a short idle
;; and reliably updates as you move between symbols.  f3 family preserved:
;;   C-f3 toggle a sticky highlight, f3/S-f3 jump next/prev, M-f3 rename.
(use-package symbol-overlay
  :ensure t
  :diminish ""
  :hook (prog-mode . symbol-overlay-mode)
  :custom (symbol-overlay-idle-time 1.0)
  :bind (([(control f3)] . symbol-overlay-put)
         ([f3]           . symbol-overlay-jump-next)
         ([(shift f3)]   . symbol-overlay-jump-prev)
         ([(meta f3)]    . symbol-overlay-rename)))

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
  (add-hook 'find-file-hook #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook (lambda () (bm-buffer-save-all) (bm-repository-save)))
  ;; bm marks are a SEPARATE system from Emacs bookmark.el (which
  ;; consult-bookmark reads), so they never show up there.  M-f2 lists the bm
  ;; marks across all buffers in a navigable buffer.
  :bind (("<f2>"   . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle)
         ("M-<f2>" . bm-show-all)
         ("<left-fringe> <mouse-1>" . bm-toggle-mouse)))

;; ---------------------------------------------------------------------------
;; Misc editing modes
;; ---------------------------------------------------------------------------
(use-package which-key :ensure t :diminish "" :config (which-key-mode))
(use-package popwin    :ensure t :config (popwin-mode 1))
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
(use-package markdown-mermaid
  :ensure t
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
