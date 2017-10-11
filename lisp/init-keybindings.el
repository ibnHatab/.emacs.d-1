;; init-keybindings.el -- Global keybindings

(define-key global-map (kbd "C-c s") (lambda () (interactive) (ansi-term "zsh")))
(define-key global-map (kbd "s-e") 'eval-buffer)
(define-key global-map (kbd "C-}") 'air-cycle-theme)

(evil-define-key 'insert global-map (kbd "C-v") 'simpleclip-paste)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
