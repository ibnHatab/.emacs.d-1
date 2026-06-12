(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-buffer-persistence t)
 '(bm-cycle-all-buffers t)
 '(bm-highlight-style 'bm-highlight-only-fringe)
 '(bm-repository-file "~/.emacs.d/bm-repository")
 '(ccls-args nil)
 '(ccls-executable "/home/axadmin/bin/ccls")
 '(doxymacs-doxygen-style "C++")
 '(doxymacs-member-comment-start nil)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-sideline-actions-icon nil)
 '(org-export-backends '(ascii html md))
 '(org-support-shift-select t)
 '(package-selected-packages
   '(;; NOTE: vertico/consult/marginalia/orderless/corfu/cape/embark and
     ;; lsp-mode/lsp-ui are NOT here -- they are pinned git checkouts under
     ;; site-lisp/ (their latest releases need Emacs 29.1; we run 28.1).
     ;; UI / shared deps
     compat doom-modeline nerd-icons color-theme-sanityinc-solarized
     rainbow-delimiters which-key diminish
     ;; editing
     multiple-cursors expand-region yasnippet yasnippet-snippets ace-jump-mode
     undo-tree comment-dwim-2 highlight-symbol bm popwin writegood-mode
     yaml-mode markdown-mode protobuf-mode
     ;; project / vcs / search
     projectile magit git-gutter ag wgrep wgrep-ag neotree
     ;; languages + lsp deps
     ccls cmake-mode cmake-font-lock clang-format
     dash f ht spinner lv
     go-mode go-eldoc go-guru gorepl-mode go-playground
     elpy py-yapf
     org-re-reveal org-bullets org-pomodoro htmlize
     flycheck let-alist
     ;; claude-code-ide deps
     vterm websocket
     ;; misc
     gist dictionary s use-package))
 '(projectile-project-root-files-top-down-recurring
   '("compile_commands.json" ".ccls" ".svn" "CVS" "Makefile"))
 '(tramp-default-host "bigdata"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
