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

(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/org/brain")
  ;; For Evil users
  ;; (eval-after-load 'evil
  ;;   (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  ;; (push '("b" "Brain" plain (function org-brain-goto-end)
  ;;         "* %i%?" :empty-lines 1))
        ;; org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))

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
(setq org-agenda-files '(gtd-file work-status-file))

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file gtd-file)))
(global-set-key (kbd "C-c w")
                (lambda () (interactive) (find-file work-status-file)))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c <up>") 'org-timestamp-up-day)
(global-set-key (kbd "C-c <down>") 'org-timestamp-down-day)

(setq org-default-notes-file gtd-file)

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(provide 'init-org)
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline gtd-file "Tasks")
                               "* TODO %i%?")))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "REVIEWING(r)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))

(provide 'init-org)
;;; init-org.el ends here
