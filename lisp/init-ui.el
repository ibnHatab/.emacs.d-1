;;; init-ui.el --- Look and feel: theme, modeline, fonts, line numbers -*- lexical-binding: t; -*-
;;; Commentary:
;; Everything visual lives here.  Keeps the long-standing
;; sanityinc-solarized-dark theme, pairs it with a working doom-modeline
;; (the old spaceline config was never actually loaded), uses the built-in
;; display-line-numbers, and preserves the Hack font + s-=/s-- resize keys.
;;; Code:

;; ---------------------------------------------------------------------------
;; Theme: Solarized Dark (unchanged)
;; ---------------------------------------------------------------------------
(use-package color-theme-sanityinc-solarized
  :ensure t
  :config
  (load-theme 'sanityinc-solarized-dark t))

;; ---------------------------------------------------------------------------
;; Modeline
;; ---------------------------------------------------------------------------
(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 22)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-icon (display-graphic-p)))

;; ---------------------------------------------------------------------------
;; Line numbers (built-in; replaces the old linum-off.el)
;; ---------------------------------------------------------------------------
(setq-default display-line-numbers-width 3)
(dolist (hook '(prog-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
;; Explicitly off where it adds noise.
(dolist (hook '(org-mode-hook text-mode-hook term-mode-hook
                shell-mode-hook eshell-mode-hook compilation-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

;; ---------------------------------------------------------------------------
;; Fonts (Hack), with s-=/s-- to resize on the fly
;; ---------------------------------------------------------------------------
(defun my/font-name-replace-size (font-name new-size)
  "Return FONT-NAME with its pixel/point size set to NEW-SIZE."
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun my/set-frame-font-size (size)
  "Set the default frame font to SIZE points."
  (set-frame-font (my/font-name-replace-size (face-font 'default) size) t t))

(defun my/increment-font-height (delta)
  "Adjust the default font height by DELTA (multiple of 10)."
  (let* ((new-height (+ (face-attribute 'default :height) delta)))
    (my/set-frame-font-size (/ new-height 10))
    (set-face-attribute 'default nil :height new-height)
    (message "Default font size is now %d" (/ new-height 10))))

(defun my/increase-font-height () (interactive) (my/increment-font-height 10))
(defun my/decrease-font-height () (interactive) (my/increment-font-height -10))
(global-set-key (kbd "s-=") #'my/increase-font-height)
(global-set-key (kbd "s--") #'my/decrease-font-height)

;; Apply the base font once a graphical frame exists.
(defun my/apply-default-font (&optional frame)
  "Apply the Hack font to FRAME (or the current frame) when on a GUI."
  (when (display-graphic-p frame)
    (set-face-attribute 'default frame :font "Hack" :height 150)))
(add-hook 'after-make-frame-functions #'my/apply-default-font)
(add-hook 'window-setup-hook #'my/apply-default-font)

;; ---------------------------------------------------------------------------
;; Misc visual niceties
;; ---------------------------------------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(column-number-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)

(provide 'init-ui)
;;; init-ui.el ends here
