;;; init-org.el -- org-mode related stuff
;;;
;;; Code: down below
;;;
;;; Commentary: org-mode dependent configuration and packages

;; Begin org-mode related packages

(use-package org-re-reveal
  :ensure t
  :config
  (setq org-re-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.7.0/"))

(use-package htmlize
  :ensure t)

(use-package ox-rst
  :ensure t)

(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-pomodoro
  :bind ("C-x C-p" . org-pomodoro)
  :ensure t)

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading)))))

(use-package hideshow-org
  :ensure t)
;; End org-mode related packages

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t) (ruby . t) (shell . t) (C . t))))

(setq org-confirm-babel-evaluate nil)

(when (version< "9.1.4" (org-version))
  (add-to-list 'org-modules 'org-tempo))

(setq gtd-file "~/org/gtd.org")
(setq work-status-file "~/org/work_status.org")
(setq org-agenda-files `(,gtd-file ,work-status-file))

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file gtd-file)))
(global-set-key (kbd "C-c w")
                (lambda () (interactive) (find-file work-status-file)))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-default-notes-file gtd-file)

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(provide 'init-org)
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline gtd-file "Tasks")
                               "* TODO %i%?")))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "REVIEWING(r)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))

(provide 'init-org)
;;; init-org.el ends here
