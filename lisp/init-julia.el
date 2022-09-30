;;; init-julia.el -- julia config
;;;
;;; Code: down below
;;;
;;; Commentary: julia dependent configuration and packages

(use-package eglot-jl
  :ensure t
  :defer  t)

(use-package julia-mode
  :ensure t
  :interpreter ("julia" . julia-mode)

  :config
  (add-hook 'julia-mode-hook 'eglot-jl-init)
  (add-hook 'julia-mode-hook 'eglot-ensure))

(require 'eglot)
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c d") 'eldoc)
(define-key eglot-mode-map (kbd "M-?") 'xref-find-references)


(use-package julia-repl
  :ensure t)

(add-hook 'julia-mode-hook 'julia-repl-mode)

(setenv "JULIA_NUM_THREADS" "8")
(setenv "JULIA_EDITOR" "emacsclient")

(provide 'init-julia)
;;; init-julia.el ends here



;; (use-package julia-mode
;;   :ensure t)


;; (add-hook 'julia-mode-hook #'lsp-mode)



;; FIXME: LSP

;; (setq lsp-julia-package-dir nil)

;; (use-package lsp-julia
;;   :ensure nil)

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







;; (defcustom julia-default-environment "~/.julia/environment/v1.0"
;;   "The default julia environment"
;;   :type 'string
;;   :group 'julia-config)
