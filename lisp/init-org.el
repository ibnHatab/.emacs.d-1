;;; init-org.el --- org-mode related stuff -*- lexical-binding: t; -*-
;;; Commentary:
;; org-mode editing niceties.  GTD/agenda/capture and the reveal.js exporter
;; were removed; this keeps only htmlize, org-bullets, babel and org-tempo.
;;; Code:

(use-package htmlize
  :ensure t)

(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t) (ruby . t) (shell . t) (C . t))))

(setq org-confirm-babel-evaluate nil)

(when (version< "9.1.4" (org-version))
  (add-to-list 'org-modules 'org-tempo))

(provide 'init-org)
;;; init-org.el ends here
