;;; init-python.el -- python config
;;;
;;; Code: down below
;;;
;;; Commentary: python dependent configuration and packages

(use-package py-yapf :ensure t)

(use-package elpy
  :ensure t
  :config
  (setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-completion-native-enable nil)
  (elpy-enable))

(add-hook 'elpy-mode-hook #'hs-minor-mode)

(add-hook 'python-mode-hook 'hs-minor-mode
	  (lambda ()
        (flycheck-mode -1)
	    (setq indent-tabs-mode nil)
	    (setq tab-width 4)
	    (setq python-indent-offset 4))
          (lambda ()
            ;; I'm rudely redefining this function to do a comparison of `point'
            ;; to the end marker of the `comint-last-prompt' because the original
            ;; method of using `looking-back' to match the prompt was never
            ;; matching, which hangs the shell startup forever.
            (defun python-shell-accept-process-output (process &optional timeout regexp)
              "Redefined to actually work."
              (let ((regexp (or regexp comint-prompt-regexp)))
                (catch 'found
                  (while t
                    (when (not (accept-process-output process timeout))
                      (throw 'found nil))
                    (when (= (point) (cdr (python-util-comint-last-prompt)))
                      (throw 'found t))))))

            ;; Additional settings follow.
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))


(provide 'init-python)
;;; init-python.el ends here
