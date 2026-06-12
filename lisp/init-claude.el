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
(use-package vterm
  :ensure t
  :defer t)

;; WebSocket transport for the MCP server.
(use-package websocket
  :ensure t
  :defer t)

;; The package is loaded from the local checkout under tmp/.  When you later
;; move it to a permanent location, just update this path (or switch to the
;; :vc form shown in the comment below).
(let ((claude-ide-dir (expand-file-name "tmp/claude-code-ide" user-emacs-directory)))
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
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  ;; Expose Emacs capabilities (xref, project, imenu, tree-sitter) to Claude.
  (when (fboundp 'claude-code-ide-emacs-tools-setup)
    (claude-code-ide-emacs-tools-setup)))

(provide 'init-claude)
;;; init-claude.el ends here
