;;; init-python.el --- Python support -*- lexical-binding: t; -*-
;;; Commentary:
;; Python editing via lsp-mode + pyright (replaces elpy).  pyright provides
;; completion, navigation and type checking; format with py-yapf, fold via
;; hideshow.  Requires the `pyright' language server on PATH
;; (npm install -g pyright).
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

;; pyright via lsp-mode.  lsp-pyright registers the client; lsp-deferred starts
;; it when a Python buffer opens, feeding completion into corfu/cape.
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :custom
  (lsp-pyright-typechecking-mode "basic"))

(add-hook 'python-mode-hook #'hs-minor-mode)

(provide 'init-python)
;;; init-python.el ends here
