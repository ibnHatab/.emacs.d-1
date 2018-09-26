;;; init-org.el -- org-mode related stuff
;;;
;;; Code: down below
;;; 
;;; Commentary:

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t) (ruby . t) (shell . t))))

(setq org-agenda-files '("~/org/gtd/inbox.org"
                         "~/org/gtd/gtd.org"
                         "~/org/work_status.org"
                         "~/org/gtd/tickler.org"))

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/org/gtd/gtd.org")))
(global-set-key (kbd "C-c w")
                (lambda () (interactive) (find-file "~/org/work_status.org")))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c <up>") 'org-timestamp-up-day)
(global-set-key (kbd "C-c <down>") 'org-timestamp-down-day)

(setq org-default-notes-file "~/org/gtd/gtd.org")

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(provide 'init-org)
;;; init-org.el ends here
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/org/gtd/gtd.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/org/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(provide 'init-org)
