;; init-keybindings.el -- Global keybindings

(define-key global-map (kbd "C-c s") (lambda () (interactive) (ansi-term "zsh")))
(define-key global-map (kbd "C-M-r") 'eval-buffer)
(define-key global-map (kbd "<f3>") 'previous-error)
(define-key global-map (kbd "<f4>") 'next-error)
(define-key global-map (kbd "C-}") 'air-cycle-theme)
(define-key global-map (kbd "C-{") 'cycle-powerline-separators)

(evil-define-key 'insert global-map (kbd "C-v") 'simpleclip-paste)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
