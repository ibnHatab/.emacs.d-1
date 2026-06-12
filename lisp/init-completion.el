;;; init-completion.el --- Minibuffer & in-buffer completion -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern completion stack replacing the old Helm + IDO + company + counsel
;; setup.  Minibuffer: vertico + orderless + marginalia + consult.  In-buffer:
;; corfu + cape.  Long-standing keybindings (M-x, C-x b, C-x C-f, C-escape,
;; M-s o) are preserved but retargeted onto the new commands.
;;
;; NOTE: the latest releases of these packages require Emacs >= 29.1, but we
;; run Emacs 28.1, so they are pinned to their last 28-compatible tags and
;; loaded from site-lisp/ (managed as git checkouts) rather than from MELPA.
;; Their only external dependency, `compat', still comes from package.el.
;;; Code:

;; Shared dependency for the pinned packages.
(use-package compat :ensure t)

;; Make the pinned checkouts loadable.
(let ((site (expand-file-name "site-lisp" user-emacs-directory)))
  (dolist (d '("vertico" "vertico/extensions" "consult" "marginalia"
               "orderless" "corfu" "corfu/extensions" "cape" "embark"))
    (let ((dir (expand-file-name d site)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; ---------------------------------------------------------------------------
;; Vertico: vertical completion UI in the minibuffer
;; ---------------------------------------------------------------------------
(use-package vertico
  :ensure nil
  :demand t
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  :config (vertico-mode))

;; Save minibuffer history (used by vertico to surface recent picks first).
(use-package savehist
  :ensure nil
  :init (savehist-mode))

;; ---------------------------------------------------------------------------
;; Orderless: space-separated, order-independent fuzzy matching
;; ---------------------------------------------------------------------------
(use-package orderless
  :ensure nil
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ---------------------------------------------------------------------------
;; Marginalia: rich annotations in the minibuffer (replaces helm details)
;; ---------------------------------------------------------------------------
(use-package marginalia
  :ensure nil
  :demand t
  :config (marginalia-mode))

;; ---------------------------------------------------------------------------
;; Consult: practical search/navigation commands (replaces helm-* + counsel)
;; ---------------------------------------------------------------------------
(use-package consult
  :ensure nil
  :demand t
  :bind (("C-x b"     . consult-buffer)            ; was helm-buffers-list
         ([C-escape]  . consult-buffer)            ; was helm-buffers-list
         ("M-s o"     . consult-line)              ; was helm-occur
         ("M-y"       . consult-yank-pop)
         ("C-x r b"   . consult-bookmark)
         ("M-g g"     . consult-goto-line)
         ("M-g i"     . consult-imenu)
         ("C-c f"     . consult-ripgrep))
  :custom
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;; ---------------------------------------------------------------------------
;; Embark: act on the thing at point / in the minibuffer
;; ---------------------------------------------------------------------------
(use-package embark
  :ensure nil
  :demand t
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure nil
  :demand t
  :after (embark consult))

;; ---------------------------------------------------------------------------
;; In-buffer completion: corfu + cape (replaces company)
;; ---------------------------------------------------------------------------
(use-package corfu
  :ensure nil
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (tab-always-indent 'complete)
  :bind (:map corfu-map
         ("<escape>" . corfu-quit)
         ("C-n" . corfu-next)
         ("C-p" . corfu-previous))
  :config (global-corfu-mode))

(use-package cape
  :ensure nil
  :demand t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(provide 'init-completion)
;;; init-completion.el ends here
