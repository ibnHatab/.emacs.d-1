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
  ;; The sort functions live in the vertico-sort extension; require it so
  ;; `vertico-sort-history-length-alpha' is bound before we reference it.
  :init (require 'vertico-sort)
  :custom
  (vertico-cycle t)
  (vertico-count 15)            ; show up to 15 candidate lines...
  (vertico-resize nil)          ; ...and keep that height fixed (don't shrink
                                ; to 3 lines for short lists)
  ;; Sort candidates by history (MRU) first, then length, then alpha.  This is
  ;; vertico's default, set explicitly so the M-x most-recently-used ordering
  ;; is documented and stable; it pairs with savehist persisting the history.
  (vertico-sort-function #'vertico-sort-history-length-alpha)
  :config
  (vertico-mode)
  ;; The minibuffer window must be allowed to grow tall enough to show the
  ;; candidates; a low `max-mini-window-height' is what caps the list at ~3.
  (setq resize-mini-windows t
        max-mini-window-height 0.4))

;; Save minibuffer history (used by vertico to surface recent picks first).
;; Persisting `extended-command-history' is what makes M-x list your
;; most-recently-used commands at the top across restarts; vertico's default
;; sort (history, then length, then alpha) does the rest.
(use-package savehist
  :ensure nil
  :init (savehist-mode)
  :custom
  (history-length 1000)
  (savehist-additional-variables '(extended-command-history
                                   kill-ring
                                   search-ring
                                   regexp-search-ring)))

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
         ("C-`"       . consult-buffer)            ; buffer list (was C-escape)
         ("M-s o"     . consult-line)              ; was helm-occur
         ("M-y"       . consult-yank-pop)
         ("C-x r b"   . consult-bookmark)
         ("M-g g"     . consult-goto-line)
         ("M-g i"     . consult-imenu)
         ("C-c f"     . consult-ripgrep)
         ;; Inside the minibuffer, C-` must NOT re-invoke consult-buffer (that
         ;; opens a minibuffer within a minibuffer -> "Command attempted to use
         ;; minibuffer while in minibuffer").  Make it abort the current prompt.
         (:map minibuffer-local-map
          ("C-`" . abort-recursive-edit)))
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
