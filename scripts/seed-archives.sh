#!/usr/bin/env bash
# Seed package.el archive caches via curl.
#
# In this environment Emacs's own url.el cannot fetch elpa.gnu.org /
# elpa.nongnu.org (it gets empty bodies), but curl can.  Run this whenever you
# need to refresh the package index before installing new packages, then do the
# install from inside Emacs (signatures are disabled in init-elpa.el).
set -euo pipefail
ELPA="${HOME}/.emacs.d/elpa/archives"
mkdir -p "$ELPA/gnu" "$ELPA/nongnu" "$ELPA/melpa"
curl -fsS -m 90 -o "$ELPA/gnu/archive-contents"    https://elpa.gnu.org/packages/archive-contents
curl -fsS -m 90 -o "$ELPA/nongnu/archive-contents" https://elpa.nongnu.org/nongnu/archive-contents
curl -fsS -m 90 -o "$ELPA/melpa/archive-contents"  https://melpa.org/packages/archive-contents
echo "Seeded archive caches:"
wc -c "$ELPA"/*/archive-contents
