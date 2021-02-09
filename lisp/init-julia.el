;;; init-julia.el -- julia config
;;;
;;; Code: down below
;;;
;;; Commentary: julia dependent configuration and packages


(use-package julia-mode
  :ensure t)

;; FIXME: LSP

;; (setq lsp-julia-package-dir nil)

(use-package lsp-julia
  :ensure nil)

;; (eval-after-load 'lsp-julia
;;     '(defun lsp-julia--rls-command ()
;;        "The command to lauch the Julia Language Server."
;;        `(,lsp-julia-command
;;          ,@lsp-julia-flags
;;          ,(concat "-e using LanguageServer, Sockets, SymbolServer;"
;;                   " server = LanguageServer.LanguageServerInstance("
;;                   " stdin, stdout, true,"
;;                   " \"" (lsp-julia--get-root) "\","
;;                   " \"" (lsp-julia--get-depot-path) "\");"
;;                   " server.runlinter = true;"
;;                   " run(server);"))))

;; (setq lsp-folding-range-limit 100)
;; (setq lsp-julia-flags '("--startup-file=no" "--history-file=no"))





(use-package julia-repl
  :ensure t)

(add-hook 'julia-mode-hook #'lsp-mode)
(add-hook 'julia-mode-hook 'julia-repl-mode)

(setenv "JULIA_NUM_THREADS" "8")
(setenv "JULIA_EDITOR" "emacsclient")

;; (defcustom julia-default-environment "~/.julia/environment/v1.0"
;;   "The default julia environment"
;;   :type 'string
;;   :group 'julia-config)

(provide 'init-julia)
;;; init-julia.el ends here
