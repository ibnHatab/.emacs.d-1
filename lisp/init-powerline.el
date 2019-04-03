;; init-powerline.el --- Powerline configuration

;;; Commentary:

;;; Code:

(use-package spaceline
  :ensure t
  :defer t
  :init
  (progn
    (require 'spaceline-config)
    (setq powerline-height '20)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (setq spaceline-workspace-numbers-unicode t)
    (setq spaceline-window-numbers-unicode t)
    (setq powerline-default-separator 'zigzag)
    (spaceline-spacemacs-theme)))

(use-package nyan-mode
  :ensure t
  :defer t
  :init
  (progn
(nyan-mode 1)))

(use-package window-numbering
  :ensure t
  :init
  (progn
(window-numbering-mode t)))

(use-package all-the-icons
  :ensure t)

(provide 'init-powerline)
;;; init-powerline.el ends here
