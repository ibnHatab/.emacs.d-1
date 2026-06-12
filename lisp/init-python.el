;;; init-python.el --- Python support -*- lexical-binding: t; -*-
;;; Commentary:
;; Python editing via elpy.  Uses ipython as the shell when available, falling
;; back to plain python3 otherwise.  Formatting with py-yapf, folding via
;; hideshow.
;;; Code:

(use-package py-yapf :ensure t :defer t)

(use-package python
  :ensure nil                       ; built-in; do not try to install
  :mode ("\\.py\\'" . python-mode)
  :config
  ;; Prefer ipython when it is installed.
  (if (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "--simple-prompt -i")
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"))
  (setq indent-tabs-mode nil
        tab-width 4
        python-indent-offset 4))

(use-package elpy
  :ensure t
  :after python
  :init (advice-add 'python-mode :before #'elpy-enable)
  :config
  ;; Let flycheck handle linting instead of elpy's flymake.
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(add-hook 'python-mode-hook #'hs-minor-mode)

(provide 'init-python)
;;; init-python.el ends here
