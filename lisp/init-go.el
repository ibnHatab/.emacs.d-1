;;; init-go.el --- Go support -*- lexical-binding: t; -*-
;;; Commentary:
;; Go editing with gofmt-on-save and eldoc.  Completion comes from the global
;; corfu stack (init-completion), so the old auto-complete/go-autocomplete
;; packages are dropped.  LSP via lsp-mode kicks in through prog-mode-hook.
;;; Code:

(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path (expand-file-name "~/.local/go/bin"))
(setenv "GOPATH" (expand-file-name "~/.local/go"))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (defun my-go-mode-hook ()
    (setq gofmt-command "goimports")          ; format + manage imports
    (add-hook 'before-save-hook #'gofmt-before-save nil t)
    (unless (string-match "go" compile-command)
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
    ;; Go-specific keys
    (local-set-key (kbd "M-*") 'pop-tag-mark)
    (local-set-key (kbd "M-p") 'compile)
    (local-set-key (kbd "M-P") 'recompile)
    (local-set-key (kbd "M-]") 'next-error)
    (local-set-key (kbd "M-[") 'previous-error)
    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
    (local-set-key (kbd "C-c i") 'go-goto-imports)
    (local-set-key (kbd "<C-return>") 'go-run))
  (add-hook 'go-mode-hook #'my-go-mode-hook))

;; Optional Go helpers — loaded lazily, only if present.
(use-package go-eldoc    :ensure t :defer t :hook (go-mode . go-eldoc-setup))
(use-package go-guru     :ensure t :defer t)
(use-package gorepl-mode :ensure t :defer t)
(use-package go-playground :ensure t :defer t)

(provide 'init-go)
;;; init-go.el ends here
