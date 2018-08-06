;;; init-org.el -- org-mode related stuff
;;;
;;; Code: down below
;;; 
;;; Commentary:

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t) (ruby . t) (shell . t))))

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"))
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/org/gtd/gtd.org")))
(global-set-key (kbd "C-c w") 
                (lambda () (interactive) (find-file "~/org/work_status.org")))
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/org/gtd/gtd.org")

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(provide 'init-org)
;;; init-org.el ends here
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))
(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(provide 'init-org)
