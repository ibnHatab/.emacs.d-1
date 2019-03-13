;; init-keybindings.el -- Global keybindings
;;;
;;; Code: down below
;;; 
;;; Commentary:

(define-key global-map (kbd "C-c s") (lambda () (interactive) (ansi-term "zsh")))
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x p i") 'org-cliplink)
(define-key global-map (kbd "C-M-r") 'eval-buffer)
(define-key global-map (kbd "<f3>") 'previous-error)
(define-key global-map (kbd "<f4>") 'next-error)
(define-key global-map (kbd "H-<left>") 'previous-error)
(define-key global-map (kbd "H-<right>") 'next-error)
(define-key global-map (kbd "C-}") 'air-cycle-theme)
(define-key global-map (kbd "C-{") 'cycle-powerline-separators)
(define-key global-map (kbd "M-\\") 'shell-command-on-region)

(define-key global-map (kbd "H-g") 'gomoku)
(define-key global-map (kbd "H-m") 'mines)
(define-key global-map (kbd "H-p") 'pacmacs)
(define-key global-map (kbd "H-d") 'dunnet)
(define-key global-map (kbd "H-t") 'tetris)
(define-key global-map (kbd "H-]") 'studlify-buffer)
(define-key global-map (kbd "H-[") 'nato-region)

(define-key global-map (kbd "C-x <up>") 'windmove-up)
(define-key global-map (kbd "C-x <down>") 'windmove-down)
(define-key global-map (kbd "C-x <left>") 'windmove-left)
(define-key global-map (kbd "C-x <right>") 'windmove-right)

(evil-define-key 'insert global-map (kbd "H-v") 'yank)
(evil-define-key 'insert global-map (kbd "H-x") 'kill-region)
(evil-define-key 'insert global-map (kbd "H-c") 'kill-ring-save)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
