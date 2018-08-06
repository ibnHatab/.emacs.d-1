;;; init-org.el -- org-mode related stuff
;;;
;;; Code: down below
;;; 
;;; Commentary:

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t) (ruby . t) (shell . t))))

(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/org/gtd/gtd.org")))
(global-set-key (kbd "C-c w") 
                (lambda () (interactive) (find-file "~/org/work_status.org")))
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/org/gtd/gtd.org")

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(provide 'init-org)
;;; init-org.el ends here
