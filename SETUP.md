# Emacs config — modernized layout

Rewritten 2026-06-12. Old config backed up to `init.el.bak` and `lisp.bak/`;
the Emacs-27 package set is preserved in `elpa.emacs27.bak/`.

## Status: working

- **Emacs 28.1** installed (kelleyk PPA `emacs28`; the PPA had no `emacs29`
  for Ubuntu 22.04). 28.1 satisfies claude-code-ide (needs >= 28.1).
- Config loads cleanly with no errors. Verified features: vertico, marginalia,
  consult, corfu, embark, doom-modeline, magit, projectile, lsp-mode,
  yasnippet, multiple-cursors, claude-code-ide, line-numbers, solarized-dark.
- `claude-code-ide-menu` bound to `C-c C-'`; the `claude` CLI is on PATH.

## Emacs 28 vs 29 — pinned packages

The latest releases of the completion stack (vertico, consult, marginalia,
corfu, cape, embark) **and** lsp-mode/lsp-ui now require Emacs 29.1. Since we
run 28.1, these are pinned to their last 28-compatible git tags under
`site-lisp/` and loaded from there instead of MELPA:

| Package | Pinned tag |
|---------|-----------|
| vertico | 2.5 |
| consult | 2.8 |
| marginalia | 2.3 |
| corfu | 2.3 |
| cape | 2.1 |
| embark | 1.1 |
| orderless | 1.7 |
| lsp-mode | 10.0.0 |
| lsp-ui | 9.0.0 |
| markdown-mermaid | 203c211 (commit) |

Everything else comes from MELPA/GNU ELPA via package.el. If you later move to
Emacs 29+, you can delete `site-lisp/` and add these back to
`package-selected-packages` to get the newest versions.

## Network workaround (this environment)

Emacs's `url.el` cannot fetch `elpa.gnu.org` / `elpa.nongnu.org` here (empty
response bodies), though `curl` and MELPA work fine. Two accommodations:

1. `scripts/seed-archives.sh` re-seeds the archive index caches via curl. Run
   it before installing new packages, then install from inside Emacs.
2. `package-check-signature` is set to `nil` in `init-elpa.el` (GNU `.sig`
   downloads come back empty and would abort installs). Re-enable to
   `'allow-unsigned` on a network where GNU ELPA works.

## Remaining manual steps

1. **vterm** compiles a native module on first use. Build deps are already
   installed (cmake, libtool-bin, libvterm-dev).
2. **doom-modeline icons** (one-time, inside Emacs): `M-x nerd-icons-install-fonts`

## Markdown + Mermaid preview

`markdown-mermaid` renders ```` ```mermaid ```` blocks via the `mmdc` CLI
(`@mermaid-js/mermaid-cli`, already installed under nvm). In a `.md` buffer,
put the cursor inside a mermaid block and press **`C-c m`** (or
`M-x markdown-mermaid-preview`) — a `*mermaid-image*` buffer opens with the
rendered PNG, themed to match solarized-dark. Errors go to `*mermaid-error*`.

Note: `init-editing.el` adds nvm's `node/*/bin` to `exec-path` so a desktop-
launched Emacs can find `mmdc`. If you upgrade Node via nvm, the glob picks the
newest version automatically.

## Layout

| File | Purpose |
|------|---------|
| `early-init.el`        | UI chrome off + GC tuning before first frame |
| `init.el`              | Slim orchestrator; requires the modules below |
| `lisp/init-elpa.el`    | Package archives (HTTPS) + use-package bootstrap |
| `lisp/init-ui.el`      | Theme (solarized-dark), doom-modeline, fonts, line numbers |
| `lisp/init-completion.el` | vertico + consult + marginalia + orderless + corfu + cape |
| `lisp/init-editing.el` | mc, expand-region, yasnippet, ace-jump, undo-tree, bm, highlight-symbol |
| `lisp/init-project.el` | projectile, magit, git-gutter, ag/wgrep, neotree, tramp |
| `lisp/init-keybindings.el` | global keys (function keys, super-window mgmt) |
| `lisp/init-platform.el`| window/buffer cycling + swap helpers |
| `lisp/init-claude.el`  | claude-code-ide + MCP tools |
| `lisp/init-{cpp,go,python,org}.el` | per-language config |
| `lisp/init-{flycheck,git,utils}.el` | linting, gitconfig mode, helper fns |
| `scripts/seed-archives.sh` | curl-seed package archive caches |

## What changed from the old config

- **Completion**: Helm + IDO + company + counsel  →  vertico stack + corfu.
  Bindings preserved: `M-x`, `C-x b`, `C-x C-f`, `C-escape`, `M-s o`, `f9`.
- **Modeline**: dead spaceline config  →  doom-modeline (now actually loaded).
- **Line numbers**: old `linum-off.el`  →  built-in `display-line-numbers`.
- **Removed dead/unused**: init-powerline, init-linum, init-fonts, init-global,
  init-gtags, init-julia, evil-org (stray evil dep), redo.el, smartparens.
- **Hardened**: doxymacs/xah-lookup load only if present; `python`/`tramp`
  no longer try to `:ensure` built-ins; HTTPS-only archives.
- Kept verbatim: all function keys, super-key window management, C-h/M-h remaps,
  git bindings, yasnippet, bm bookmarks, language modules.

## Rollback

```sh
cd ~/.emacs.d
cp init.el.bak init.el
rm -rf lisp && mv lisp.bak lisp
rm -rf elpa && mv elpa.emacs27.bak elpa   # restore Emacs-27 packages
# and reinstall emacs27 if desired: sudo apt-get install emacs (after removing emacs28)
```
