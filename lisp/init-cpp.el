;;; init-cpp.el --- C/C++ settings -*- lexical-binding: t; -*-
;;; Commentary:
;; C/C++ IDE support built around clangd + compile_commands.json (compdb).
;; lsp-mode feeds completion into the corfu/cape stack (see init-completion.el).
;; lsp-mode/lsp-ui install from MELPA on Emacs 30.
;;; Code:

;; lsp-mode: clangd discovers compile_commands.json by walking up from the
;; source file, so no compdb path config is needed here.
(use-package lsp-mode
  :ensure t
  :hook ((c-mode c++-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-ui-doc-enable nil)
  ;; Use clangd (not ccls) for C/C++: it reads compile_commands.json directly.
  (lsp-disabled-clients '(ccls))
  ;; Let clangd insert headers itself; -j and background indexing keep
  ;; completion responsive on large compdb projects.
  (lsp-clients-clangd-args '("--header-insertion=iwyu"
                             "--background-index"
                             "-j=4"
                             "--header-insertion-decorators=0"))
  :config
  ;; lsp-mode does NOT honour .gitignore for file-watching.  This helper reads
  ;; the project's .gitignore and adds its *directory* entries to the GLOBAL
  ;; `lsp-file-watch-ignored-directories'.  Two things matter for it to work:
  ;;
  ;;   * It must run on `lsp-before-initialize-hook' -- that fires BEFORE the
  ;;     watch is registered (and before the "watch N directories?" prompt).
  ;;     The old `lsp-after-open-hook' ran too late, so the prompt still showed.
  ;;   * It must modify the GLOBAL value, not a buffer-local one.  lsp reads the
  ;;     ignore list inside its own temp buffer at the workspace root
  ;;     (`lsp--get-ignored-regexes-for-workspace-root'), so a `setq-local' in
  ;;     the source buffer never reaches it.
  ;;
  ;; NOTE: watch-ignoring a dir only stops file-change *watching*; it does NOT
  ;; hide the dir from the language server's analysis.  So ignoring /.venv/
  ;; here does not stop pyright from using the venv -- pyright still resolves
  ;; imports from it; we just don't burn inotify watches on it.
  (defun my/lsp-ignore-gitignored-dirs ()
    "Add directory patterns from the current project's .gitignore to the
global `lsp-file-watch-ignored-directories'."
    (when-let* ((root (locate-dominating-file default-directory ".gitignore")))
      (with-temp-buffer
        (insert-file-contents (expand-file-name ".gitignore" root))
        (dolist (raw (split-string (buffer-string) "\n" t))
          (let ((line (string-trim raw)))
            ;; skip comments, blanks, and negations (!unignore)
            (unless (or (string-empty-p line)
                        (string-prefix-p "#" line)
                        (string-prefix-p "!" line))
              (let* ((name (replace-regexp-in-string "/+$" "" line))
                     (base (file-name-nondirectory
                            (replace-regexp-in-string "^/+" "" name))))
                ;; only plain dir names, not glob/file patterns (*.pyc, core.*)
                (when (and (not (string-empty-p base))
                           (not (string-match-p "[*?.]" base)))
                  (add-to-list 'lsp-file-watch-ignored-directories
                               (concat "[/\\\\]" (regexp-quote base) "\\'"))))))))))
  ;; Runs before watch registration, for ANY repo with a .gitignore.
  (add-hook 'lsp-before-initialize-hook #'my/lsp-ignore-gitignored-dirs))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :ensure t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package clang-format
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

(defun my-c++-mode-hook ()
  (c-toggle-auto-hungry-state 1)
  (fset 'c-indent-region 'clang-format-region)
  (auto-fill-mode))

(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer nil t))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'clang-format-buffer-smart-on-save)

;; autoinsert C/C++ header
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
  '(nil
	"///\n"
	"/// @file " (file-name-nondirectory buffer-file-name) "\n"
	"/// @author <smp28rd> \n"
	"///\n"
	"/// @brief\n"
	"///\n"
	"///\n"
	"///\n"
	(make-string 70 ?/) "\n\n"
	(let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
		   (nopath (file-name-nondirectory noext))
		   (ident (concat "__" (upcase nopath) "_H__")))
	  (concat "#ifndef " ident "\n"
			  "#define " ident  "\n\n\n"
			  "\n\n#endif // " ident "\n"))
	))

;; auto insert C/C++
(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++ implementation")
  '(nil
	"///\n"
	"/// @file " (file-name-nondirectory buffer-file-name) "\n"
	"/// @author <smp28rd> \n"
	"///\n"
	"/// @brief\n"
	"///\n"
	"///\n"
	(make-string 70 ?/) "\n\n"
	(let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
		   (nopath (file-name-nondirectory noext))
		   (ident (concat nopath ".h")))
	  (if (file-exists-p ident)
		  (concat "#include \"" ident "\"\n")))
	(make-string 70 ?/) "\n"
	))

;; c++-mode-map keys (set once cc-mode is loaded so the map exists).
(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "C-c C-h") #'ff-find-other-file))

(provide 'init-cpp)
;;; init-cpp.el ends here
