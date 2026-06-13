;;; init-keybindings.el --- Global keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;; Global key map preserving long-standing muscle memory: function keys,
;; super-key window management, and the C-h/M-h remaps.  Completion and
;; package-specific keys live in their own modules (init-completion, etc.).
;;; Code:

;; ---------------------------------------------------------------------------
;; Terminal / misc
;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-c s") (lambda () (interactive) (vterm)))
(global-set-key (kbd "M-\\")  'shell-command-on-region)
(global-set-key (kbd "M-,")   'pop-tag-mark)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-c C-c") 'projectile-compile-project)

;; ---------------------------------------------------------------------------
;; Function keys
;; ---------------------------------------------------------------------------
(global-set-key (kbd "<f1>")    'switch-to-buffer-other-buffer)
(global-set-key (kbd "<f11>")   'switch-to-buffer-other-buffer)
(global-set-key (kbd "<f4>")    'next-error)
(global-set-key (kbd "<f12>")   'kill-this-buffer)
(global-set-key (kbd "C-<f12>") 'server-edit)
(global-set-key (kbd "s-<left>")  'previous-error)
(global-set-key (kbd "s-<right>") 'next-error)

;; ---------------------------------------------------------------------------
;; Text motion / editing
;; ---------------------------------------------------------------------------
(global-set-key [M-S-up]   'move-text-up)
(global-set-key [M-S-down] 'move-text-down)
(global-set-key (kbd "C-<backspace>") 'contextual-backspace)
(global-set-key (kbd "M-+") 'toggle-identifier-naming-style)

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key [C-right]  'forward-word)
(global-set-key [C-left]   'backward-word)
(global-set-key [?\C-\.]   'goto-line)
(global-set-key [C-delete] 'kill-word)
(global-set-key [C-backspace] 'backward-kill-word)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z")     'zap-up-to-char)
(global-set-key (kbd "C-a")     'crux-move-beginning-of-line)

;; Undo / redo (undo-tree provides redo; no more redo.el).
(global-set-key (kbd "C-z")        'undo-tree-undo)
(global-set-key [M-backspace]      'undo-tree-undo)
(global-set-key [M-return]         'undo-tree-redo)

;; Editing conveniences via crux (replaces the old fragile kbd-macro string
;; that was bound to C-c C-d for duplicating a line).
(use-package crux
  :ensure t
  :bind (("C-c C-d"   . crux-duplicate-current-line-or-region)        ; was a kbd macro
         ("C-c M-d"   . crux-duplicate-and-comment-current-line-or-region)
         ([remap kill-line] . crux-smart-kill-line)                   ; C-k: kill to EOL, then whole line
         ("C-S-k"     . crux-kill-whole-line)
         ("M-o"       . crux-smart-open-line)                         ; open line below, indented
         ("C-M-o"     . crux-smart-open-line-above)
         ("C-c r"     . crux-rename-file-and-buffer)
         ("C-c D"     . crux-delete-file-and-buffer)
         ("C-c k"     . crux-kill-other-buffers)
         ("C-^"       . crux-top-join-line)))                         ; join next line up

;; ---------------------------------------------------------------------------
;; Window / buffer management (super key)
;; ---------------------------------------------------------------------------
(global-set-key [s-S-up]   'delete-other-windows-vertically)
(global-set-key [s-S-down] 'delete-other-windows-vertically)

(global-set-key [M-s-up]    'buffer-up-swap)
(global-set-key [M-s-down]  'buffer-down-swap)
(global-set-key [M-s-left]  'buffer-left-swap)
(global-set-key [M-s-right] 'buffer-right-swap)

(global-set-key (kbd "S-M-s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-M-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-M-s-<down>")  'shrink-window)
(global-set-key (kbd "S-M-s-<up>")    'enlarge-window)

(global-set-key [s-left]  'windmove-left-cycle)
(global-set-key [s-right] 'windmove-right-cycle)
(global-set-key [s-up]    'windmove-up-cycle)
(global-set-key [s-down]  'windmove-down-cycle)

(global-set-key (kbd "C-x x") 'delete-window)

;; Treemacs (also set via use-package :bind in init-project; duplicated here so
;; the bindings survive even if that block isn't re-evaluated).
(global-set-key (kbd "s-s") 'treemacs)
(global-set-key (kbd "s-S") 'treemacs-select-directory)

;; ---------------------------------------------------------------------------
;; Git
;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-x ?") 'vc-git-grep)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
