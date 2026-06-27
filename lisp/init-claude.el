;;; init-claude.el --- Claude Code IDE integration (MCP) -*- lexical-binding: t; -*-
;;; Commentary:
;; Native Claude Code integration over MCP.  Provides a bidirectional bridge
;; between Claude and Emacs (LSP/xref, project, imenu, diagnostics, ediff).
;;
;; Requirements (handled below): Emacs >= 28.1, the `claude` CLI on PATH,
;; the `websocket` package, and a terminal backend (`vterm`).  vterm needs
;; cmake + libtool + libvterm headers to compile its module the first time:
;;   sudo apt-get install -y cmake libtool-bin libvterm-dev
;;; Code:

;; Terminal backend used by claude-code-ide for the CLI session.
;; The in-vterm C-` binding lives here (not on vterm-toggle) so it is applied
;; only after vterm-mode-map actually exists; binding it on the deferred
;; vterm-toggle package fired before vterm loaded and errored with
;; "Symbol's value as variable is void: vterm-mode-map".
(use-package vterm
  :ensure t
  :defer t
  :bind (:map vterm-mode-map
              ("C-`" . vterm-toggle)))

;; VSCode-style terminal toggle: C-` shows the vterm in the lower 25% of the
;; frame; pressing it again hides it (and from inside the vterm it switches
;; back to the previous buffer).  Replaces the former C-` -> consult-buffer
;; alias (see init-completion.el).
(use-package vterm-toggle
  :ensure t
  :bind (("C-`" . vterm-toggle))
  :custom
  ;; Reuse a single terminal window at the bottom, like VSCode's panel.
  (vterm-toggle-fullscreen-p nil)
  :config
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _) (with-current-buffer bufname
                                       (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.25))))

;; WebSocket transport for the MCP server.
(use-package websocket
  :ensure t
  :defer t)

;; The package is loaded from the local checkout under local/.  When you later
;; move it to a permanent location, just update this path (or switch to the
;; :vc form shown in the comment below).
(let ((claude-ide-dir (expand-file-name "local/claude-code-ide" user-emacs-directory)))
  (when (file-directory-p claude-ide-dir)
    (add-to-list 'load-path claude-ide-dir)))

;; Alternative install once you prefer Emacs to manage it (Emacs >= 29):
;; (use-package claude-code-ide
;;   :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest))

(use-package claude-code-ide
  :ensure nil                      ; loaded from the local checkout above
  :commands (claude-code-ide
             claude-code-ide-menu
             claude-code-ide-send-prompt
             claude-code-ide-continue
             claude-code-ide-resume
             claude-code-ide-stop)
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("C-c C-;" . my/claude-ide-serve)
         ("C-c C-:" . my/claude-ide-unserve))
  :config
  ;; Expose Emacs capabilities (xref, project, imenu, tree-sitter) to Claude.
  (when (fboundp 'claude-code-ide-emacs-tools-setup)
    (claude-code-ide-emacs-tools-setup)))

;; Start ONLY the MCP/IDE WebSocket server (writes ~/.claude/ide/<port>.lock)
;; without spawning a vterm CLI session.  Then run `/ide` from a Claude Code
;; terminal started in the same project and it will discover and connect.
;; The lockfile's workspaceFolders is the project root, so launch the CLI from
;; the same project for `/ide` to match this server.
(defun my/claude-ide-serve ()
  "Start the Claude Code IDE server for the current project (no terminal).
Run `/ide' from a Claude Code CLI session in this project to connect."
  (interactive)
  (require 'claude-code-ide-mcp)
  (let* ((dir (or (when (fboundp 'claude-code-ide--get-working-directory)
                    (claude-code-ide--get-working-directory))
                  (when-let ((proj (project-current)))
                    (project-root proj))
                  default-directory))
         (port (claude-code-ide-mcp-start dir)))
    (message "Claude IDE server listening on port %d for %s (run /ide to connect)"
             port (abbreviate-file-name dir))))

(defun my/claude-ide-unserve ()
  "Stop the Claude Code IDE server for the current project."
  (interactive)
  (require 'claude-code-ide-mcp)
  (let ((dir (or (when-let ((proj (project-current))) (project-root proj))
                 default-directory)))
    (claude-code-ide-mcp-stop-session (expand-file-name dir))
    (message "Claude IDE server stopped for %s" (abbreviate-file-name dir))))

(provide 'init-claude)
;;; init-claude.el ends here
