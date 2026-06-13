;;; init-haskell.el --- Haskell editing -*- lexical-binding: t; -*-
;;; Commentary:
;; haskell-mode with indentation and the interactive GHCi REPL.  Only `ghc' is
;; installed here (no cabal/stack/HLS), so this is a lightweight setup; the
;; lsp-haskell block is left commented for when haskell-language-server exists.
;;; Code:

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'"   . haskell-mode)
         ("\\.lhs\\'"  . haskell-literate-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :hook ((haskell-mode . haskell-indentation-mode)   ; smart layout-aware indent
         (haskell-mode . interactive-haskell-mode)   ; C-c C-l loads into GHCi
         (haskell-mode . haskell-doc-mode))          ; type sigs in the echo area
  :custom
  (haskell-process-type 'ghci)                       ; plain ghci; no stack/cabal
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  :bind (:map haskell-mode-map
         ("C-c C-l" . haskell-process-load-file)
         ("C-c C-z" . haskell-interactive-switch)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-c" . haskell-process-cabal-build)))

;; LSP via haskell-language-server (install HLS first, then uncomment):
;; (use-package lsp-haskell
;;   :ensure t
;;   :hook (haskell-mode . lsp-deferred))

(provide 'init-haskell)
;;; init-haskell.el ends here
