;;; init-platform --- Platform-specific settings
;;; Commentary:

;;; Code:
(require 'init-fonts)

;; This must run after window setup or it seems to have no effect.
(add-hook 'window-setup-hook
          (lambda ()
            (when (memq window-system '(x))
              (add-to-list 'default-frame-alist '(font . "Hack"))
              (set-face-attribute 'default nil :font "Hack")
              (sanityinc/set-frame-font-size 16))
            (when (fboundp 'powerline-reset)
              (powerline-reset))))

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     ((and (boundp 'subword-mode)
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))


;; Camel / Uncamel cases
(defun toggle-identifier-naming-style ()
  "Toggles the symbol at point between C-style naming,
  e.g. `hello_world_string', and camel case,
  e.g. `HelloWorldString'."
  (interactive)
  (let* ((symbol-pos (bounds-of-thing-at-point 'symbol))
         case-fold-search symbol-at-point cstyle regexp func)
    (unless symbol-pos
      (error "No symbol at point"))
    (save-excursion
      (narrow-to-region (car symbol-pos) (cdr symbol-pos))
      (setq cstyle (string-match-p "_" (buffer-string))
            regexp (if cstyle "\\(?:\\_<\\|_\\)\\(\\w\\)" "\\([A-Z]\\)")
            func (if cstyle
                     'capitalize
                   (lambda (s)
                     (concat (if (= (match-beginning 1)
                                    (car symbol-pos))
                                 ""
                               "_")
                             (downcase s)))))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match (funcall func (match-string 1))
                       t nil))
      (widen))))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill term buffer when term is ended."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(defun switch-to-buffer-other-buffer ()
  ""
  (interactive)
  (switch-to-buffer (other-buffer)))

;; Windows Cycling
(defun windmove-up-cycle()
  (interactive)
  (condition-case nil (windmove-up)
    (error (condition-case nil (windmove-down)
	          (error (condition-case nil (windmove-right) (error (condition-case nil (windmove-left) (error (windmove-up))))))))))

(defun windmove-down-cycle()
  (interactive)
  (condition-case nil (windmove-down)
    (error (condition-case nil (windmove-up)
	          (error (condition-case nil (windmove-left) (error (condition-case nil (windmove-right) (error (windmove-down))))))))))

(defun windmove-right-cycle()
  (interactive)
  (condition-case nil (windmove-right)
    (error (condition-case nil (windmove-left)
	          (error (condition-case nil (windmove-up) (error (condition-case nil (windmove-down) (error (windmove-right))))))))))

(defun windmove-left-cycle()
  (interactive)
  (condition-case nil (windmove-left)
    (error (condition-case nil (windmove-right)
	          (error (condition-case nil (windmove-down) (error (condition-case nil (windmove-up) (error (windmove-left))))))))))

;; Buffer swaping
(defun buffer-up-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
	(progn (windmove-up-cycle)
	 (setq swaped-window (selected-window))
	 (setq swaped-buffer (buffer-name))
	 (if (and (not (string= swaped-buffer current-buffer)))
	     (progn (set-window-buffer swaped-window current-buffer)
		    (set-window-buffer current-window swaped-buffer))))))

(defun buffer-down-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
	(progn (windmove-down-cycle)
	 (setq swaped-window (selected-window))
	 (setq swaped-buffer (buffer-name))
	 (if (and (not (string= swaped-buffer current-buffer)))
	     (progn (set-window-buffer swaped-window current-buffer)
		    (set-window-buffer current-window swaped-buffer))))))

(defun buffer-right-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
	(progn (windmove-right-cycle)
	 (setq swaped-window (selected-window))
	 (setq swaped-buffer (buffer-name))
	 (if (and (not (string= swaped-buffer current-buffer)))
	     (progn (set-window-buffer swaped-window current-buffer)
		    (set-window-buffer current-window swaped-buffer))))))

(defun buffer-left-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
	(progn (windmove-left-cycle)
	 (setq swaped-window (selected-window))
	 (setq swaped-buffer (buffer-name))
	 (if (and (not (string= swaped-buffer current-buffer)))
	     (progn (set-window-buffer swaped-window current-buffer)
		    (set-window-buffer current-window swaped-buffer))))))

;; Window Dedicated Toggle Function
(defun toggle-dedicated-window ()
  (interactive)
  (let ((d (window-dedicated-p (selected-window))))
    (progn (set-window-dedicated-p (selected-window) (not d))
	   (if d (message "Window is not dedicated") (message "Window is now dedicated")))))

;; Switch window keybindings
;; (global-set-key (kbd "C-x <up>") 'windmove-up-cycle)
;; (global-set-key (kbd "C-x <down>") 'windmove-down-cycle)
;; (global-set-key (kbd "C-x <right>") 'windmove-right-cycle)
;; (global-set-key (kbd "C-x <left>") 'windmove-left-cycle)
;; (global-set-key (kbd "M-<up>") 'windmove-up-cycle)
;; (global-set-key (kbd "M-<down>") 'windmove-down-cycle)
;; (global-set-key (kbd "M-<right>") 'windmove-right-cycle)
;; (global-set-key (kbd "M-<left>") 'windmove-left-cycle)

;; Swap window keybindings
;; (global-set-key (kbd "S-M-<up>") 'buffer-up-swap)
;; (global-set-key (kbd "S-M-<down>") 'buffer-down-swap)
;; (global-set-key (kbd "S-M-<right>") 'buffer-right-swap)
;; (global-set-key (kbd "S-M-<left>") 'buffer-left-swap)

;; Window Resizing keybindings
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
;; (global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Window Split keybindings
;; (global-set-key (kbd "C-x -") 'split-window-vertically)
;; (global-set-key (kbd "C-x |") 'split-window-horizontally)

;; Window Close keybindings
(global-set-key (kbd "C-x x") 'delete-window)


(use-package rainbow-delimiters
  :ensure t
  :defer t
)


(use-package smartparens
  :ensure t
  :defer t
)

(defun my-code-mode-init ()
  (my-turn-modes 1
                 ;; 'linum-mode
                 'rainbow-delimiters-mode
                 'flycheck-mode
                 ;; 'show-paren-mode
                 'electric-indent-mode
                 'electric-pair-mode)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook
 'prog-mode-hook
 'my-code-mode-init)

(add-hook 'write-file-functions 'delete-trailing-whitespace)
(autoload 'nuke-trailing-whitespace "whitespace" nil t) ;remove trailing


(provide 'init-platform)
;;; init-platform.el ends here
