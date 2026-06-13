;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-
;;; Commentary:
;; Slim orchestrator.  Bootstraps package.el + use-package, then loads the
;; modular configuration from the lisp/ directory.  Look-and-feel lives in
;; init-ui; completion in init-completion; per-language config in init-<lang>.
;;; Code:

;; ---------------------------------------------------------------------------
;; Load paths
;; ---------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))
(dolist (dir '("/usr/local/bin" "/usr/bin"))
  (add-to-list 'exec-path dir))

;; ---------------------------------------------------------------------------
;; Backups / autosaves out of the way
;; ---------------------------------------------------------------------------
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t))
      create-lockfiles nil)

;; custom.el holds Customize output only; keep it out of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; ---------------------------------------------------------------------------
;; Package system + use-package (see init-elpa)
;; ---------------------------------------------------------------------------
(require 'init-elpa)
(require 'init-utils)         ; free functions + movement/window helpers

;; ---------------------------------------------------------------------------
;; Modules
;; ---------------------------------------------------------------------------
(require 'init-ui)            ; theme, modeline, fonts, line numbers
(require 'init-completion)    ; vertico/consult/marginalia/orderless/corfu
(require 'init-editing)       ; mc, expand-region, yasnippet, avy, undo
(require 'init-project)       ; projectile, magit, git-gutter, ag
(require 'init-keybindings)   ; global keys (after the above are loaded)
(require 'init-flycheck)
(require 'init-org)
(require 'init-go)
(require 'init-python)
(require 'init-cpp)
(require 'init-haskell)
(require 'init-claude)        ; claude-code-ide + MCP

;; ---------------------------------------------------------------------------
;; Sensible global defaults
;; ---------------------------------------------------------------------------
(setq-default tab-width 4
              indent-tabs-mode nil)
(setq scroll-step 1
      scroll-conservatively 10000)
(fset 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun my-turn-modes (param &rest modes)
  "Call each mode in MODES with PARAM."
  (mapc (lambda (mode) (funcall mode param)) modes))

(my-turn-modes 1
               'global-auto-revert-mode
               'global-hl-line-mode
               'which-key-mode
               'winner-mode
               'delete-selection-mode)

(setq-default which-key-idle-delay 0.9)

;; File associations
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.aidl\\'"   . idl-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'"    . c++-mode))

(ad-activate 'term-sentinel)

(setenv "EDITOR" "emacsclient")
(server-start)

(provide 'init)
;;; init.el ends here
