;;; init-python.el -- julia config
;;;
;;; Code: down below
;;;
;;; Commentary: julia dependent configuration and packages


(use-package julia-mode
  :ensure t)

(use-package lsp-julia
  :ensure t)

(use-package julia-repl
  :ensure t)

(add-hook 'julia-mode-hook #'lsp-mode)
(add-hook 'julia-mode-hook 'julia-repl-mode)

(setenv "JULIA_NUM_THREADS" "8")
(setenv "JULIA_EDITOR" "emacsclient")


(provide 'init-julia)
;;; init-julia.el ends here
