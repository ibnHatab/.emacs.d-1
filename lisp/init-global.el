;;; init-global.el -- Global functions used by mappings

(defun air-cycle-theme (&optional reverse)
  "Load the next (or previous if REVERSE is true) available theme."
  (interactive)
  (if (> (length custom-enabled-themes) 1)
      (message "You cannot cycle themes with more than one theme enabled")
    (let* ((current-theme (car custom-enabled-themes))
           (all-themes (if reverse
                           (reverse (custom-available-themes))
                         (custom-available-themes)))
           (first-theme (car all-themes))
           (go (lambda (theme)
                 (message "Loading %s." (symbol-name theme))
                 (disable-theme current-theme)
                 (load-theme theme)))
           theme)
      (if (catch 'done
            (while (setq theme (pop all-themes))
              (if (and (eq theme current-theme)
                       (setq theme (pop all-themes)))
                  (progn
                    (funcall go theme)
                    (throw 'done nil))))
            t)
          (funcall go first-theme)))))

(defun cycle-powerline-separators (&optional reverse)
  "Set Powerline separators in turn.  If REVERSE is not nil, go backwards."
  (interactive)
  (let* ((fn (if reverse 'reverse 'identity))
         (separators (funcall fn '("arrow" "arrow-fade" "slant"
                                   "chamfer" "wave" "brace" "roundstub" "zigzag"
                                   "butt" "rounded" "contour" "curve")))
         (found nil))
    (while (not found)
      (progn (setq separators (append (cdr separators) (list (car separators))))
             (when (string= (car separators) powerline-default-separator)
               (progn (setq powerline-default-separator (cadr separators))
                      (setq found t)
                      (redraw-display)))))))

(provide 'init-global)
;;; init-global.el ends here
