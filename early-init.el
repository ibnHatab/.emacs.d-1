;;; early-init.el --- Loaded before the GUI is initialized -*- lexical-binding: t; -*-
;;; Commentary:
;; Runs before package.el and before the first frame is drawn.  We use it
;; to disable chrome early (no flicker) and to relax the garbage collector
;; during startup for a faster launch.
;;; Code:

;; Raise the GC threshold during startup; init-elpa restores a sane value.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Don't let package.el initialize automatically; we drive it in init-elpa.
(setq package-enable-at-startup nil)

;; Disable UI chrome before the frame is created (no visual flash).
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;; Quiet startup.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      ring-bell-function 'ignore)

;; Avoid expensive frame resizing while fonts/themes load.
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here
