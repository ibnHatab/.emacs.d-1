;; init-keybindings.el -- Global keybindings
;;;
;;; Code:
;;;
;;; Commentary:

(define-key global-map (kbd "C-c s") (lambda () (interactive) (ansi-term "zsh")))

;; Move line/code region with M-S-Up/Down
(global-set-key [M-S-up]            'move-text-up)
(global-set-key [M-S-down]          'move-text-down)

(define-key global-map (kbd "<f4>")      'next-error)
(define-key global-map (kbd "s-<left>")  'previous-error)
(define-key global-map (kbd "s-<right>") 'next-error)
(define-key global-map (kbd "M-\\")      'shell-command-on-region)
(define-key global-map (kbd "\M-,")      'pop-tag-mark)
(define-key global-map (kbd "C-x \\")    'align-regexp)
(define-key global-map (kbd "C-c C-c")   'projectile-compile-project)

(define-key global-map (kbd "<f1>")      'switch-to-buffer-other-buffer)
(define-key global-map (kbd "<f11>")      'switch-to-buffer-other-buffer)
(define-key global-map (kbd "<f9>")      'ag)
(define-key global-map (kbd "<f12>")     'kill-this-buffer)
(define-key global-map (kbd "C-<f12>")   'server-edit)

;; Move line/code region with M-S-Up/Down
(global-set-key [M-S-up]                 'move-text-up)
(global-set-key [M-S-down]               'move-text-down)
(global-set-key (kbd "C-<backspace>")    'contextual-backspace)
(global-set-key "\M-+"                   'toggle-identifier-naming-style)

(global-set-key (kbd "C-h")              'delete-backward-char)
(global-set-key (kbd "M-h")              'backward-kill-word)

;; Fast movements
(global-set-key [C-right]                'forward-word)
(global-set-key [C-left]                 'backward-word)
(global-set-key [?\C-\.]                 'goto-line)
(global-set-key [C-delete]               'kill-word)
(global-set-key [ESC-backspace]          'backward-kill-word)
(global-set-key [C-backspace]            'backward-kill-word)

(global-set-key (kbd "C-x C-b")          'ibuffer)
(global-set-key (kbd "M-z")              'zap-up-to-char)
(global-set-key (kbd "M-s o")            'helm-occur)
(global-set-key (kbd "s-s")              'neotree-toggle)

;; Undo/Redo
(require 'redo)
(global-set-key [M-backspace] 'undo)
(global-set-key "\C-z"        'undo)
(global-set-key [M-return]    'redo)

;; duplicate line
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y\C-p\C-a")

;; Fast movements
(global-set-key [C-right]       'forward-word)
(global-set-key [C-left]        'backward-word)
(global-set-key [?\C-\.]        'goto-line)
(global-set-key [C-delete]      'kill-word)
(global-set-key [ESC-backspace] 'backward-kill-word)
(global-set-key [C-backspace]   'backward-kill-word)
(global-set-key [C-escape]      'helm-buffers-list)

(global-set-key (kbd "C-a")           'smart-line-beginning)

(global-set-key [s-S-up]              'delete-other-windows-vertically)
(global-set-key [s-S-down]            'delete-other-windows-vertically)

(global-set-key [M-s-up]              'buffer-up-swap)
(global-set-key [M-s-down]            'buffer-down-swap)
(global-set-key [M-s-left]            'buffer-left-swap)
(global-set-key [M-s-right]           'buffer-right-swap)

(global-set-key (kbd "S-M-s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-M-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-M-s-<down>")  'shrink-window)
(global-set-key (kbd "S-M-s-<up>")    'enlarge-window)

(global-set-key [s-left]  'windmove-left-cycle)
(global-set-key [s-right] 'windmove-right-cycle)
(global-set-key [s-up]    'windmove-up-cycle)
(global-set-key [s-down]  'windmove-down-cycle)


(global-set-key (kbd "C-x ?")   'git-grep)
(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-x p")   'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n")   'git-gutter:next-hunk)
(global-set-key (kbd "C-x r")   'git-gutter:revert-hunk)

(require `expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
