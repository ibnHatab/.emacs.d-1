;;; init-cpp.el -- c++ settings
;;;
;;; Code: down below
;;;
;;; Commentary:

(use-package xcscope
  :ensure t)

(use-package helm-cscope
  :ensure t)

(use-package ccls
  :after projectile
  :ensure t
  ;; :ensure-system-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :ensure t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-ide
  :ensure t
  :hook (c++-mode . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-compile-command (concat "cd " cmake-ide-build-dir " && make"))
    (cmake-ide-load-db))
  :bind ([remap comment-region] . cmake-ide-compile)
  :init
  (use-package semantic/bovine/gcc)
  (setq cmake-ide-flags-c++ (append '("-std=c++11")
                                    (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c++"))))
  (setq cmake-ide-flags-c (append (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c"))))
  (cmake-ide-setup)
  :config
  (put 'cmake-ide-build-dir 'safe-local-variable #'stringp))

(use-package clang-format
  :ensure t
  :init
  (defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer))))

(defun my-c++-mode-hook ()
  (c-set-style "stroustrup")
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil 'local))
  ;; (auto-fill-mode))
  ;; (c-toggle-auto-hungry-state 1))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'hs-minor-mode)

(provide 'init-cpp)
;;; init-cpp.el ends here
