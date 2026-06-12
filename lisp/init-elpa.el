;;; init-elpa.el --- Package system bootstrap -*- lexical-binding: t; -*-
;;; Commentary:
;; Sets up package.el archives (HTTPS only), bootstraps use-package, and
;; restores a sane garbage-collection threshold after the early-init bump.
;;; Code:

(require 'package)

;; HTTPS archives only.
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;; Signature verification is disabled: signed package downloads from
;; elpa.gnu.org are unreliable in this environment (empty .sig bodies),
;; which would otherwise abort installs.  Re-enable to 'allow-unsigned
;; if you move to a network where GNU ELPA downloads work cleanly.
(setq package-check-signature nil)

(package-initialize)

;; Refresh archive contents, tolerating a single archive being temporarily
;; unreachable (elpa.gnu.org / elpa.nongnu.org occasionally rate-limit).
(defun my/package-refresh-safe ()
  "Refresh package contents without aborting if one archive is down."
  (condition-case err
      (package-refresh-contents)
    (error
     (message "Package refresh: some archives unavailable (%s)" err))))

(unless package-archive-contents
  (my/package-refresh-safe))

;; Bootstrap use-package (built in on Emacs 29+, installed otherwise).
(unless (package-installed-p 'use-package)
  (my/package-refresh-safe)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Install :ensure'd packages automatically; keep startup quiet.
(setq use-package-always-ensure t
      use-package-expand-minimally t)

(use-package diminish :ensure t)

;; Restore GC behaviour after startup (early-init raised it).
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)))

(provide 'init-elpa)
;;; init-elpa.el ends here
