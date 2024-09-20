;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(defvar jd/org-home "~/Notes")
(defvar jd/org-roam-home (concat jd/org-home "/roam"))
(defvar jd/org-sync (concat jd/org-home "/sync"))
(defvar jd/org-roam-daily-home (concat jd/org-roam-home "/daily"))

(defun jd/org-mode-init ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun jd-emacs/org-insert-date (&optional date)
  (org-insert-time-stamp (org-read-date nil t (or date "+0d"))))

(use-package org-pomodoro
  :guix-package "emacs-org-pomodoro")

(use-package org-caldav
  :guix-package "emacs-org-caldav"
  :config
  (setq org-caldav-url         "http://caldav.jdlugosz.com/radicale/admin/"
	org-caldav-files       nil
	org-icalendar-timezone "Europe/Warsaw")
  
  (defun jd/caldav-calendar-sync ()
    (interactive)
    (let ((org-caldav-calendar-id "0c54a523-c7aa-2f26-2c18-a12b69c2bc86")
	  (org-caldav-inbox (concat jd/org-sync
				    "/calendar.org")))
      (org-caldav-sync)))

  (defun jd/caldav-journal-sync ()
    (interactive)
    (let ((org-caldav-calendar-id "3cc70419-a787-5f84-28c6-96f15fc606d9")
	  (org-caldav-inbox (concat jd/org-sync
				    "/journal.org")))
      (org-caldav-sync)))

  (defun jd/caldav-tasks-sync ()
    (interactive)
    (let ((org-caldav-calendar-id "372cbbb3-14f7-fc15-9f7b-cae04114920c")
	  (org-caldav-inbox (concat jd/org-sync
				    "/tasks.org")))
      (org-caldav-sync))))

(use-package org
  :guix-package "emacs-org"
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . jd/org-mode-init)
  :bind
  ("C-c o c" . #'org-capture)
  ("C-c o p" . #'org-mobile-pull)
  ("C-c o P" . #'org-mobile-push)
  ("C-c o a" . #'org-agenda)
  :config
  (defun jd/org-mode-file (file-name) (concat org-directory "/" file-name ".org"))
  (setq org-directory (file-truename "~/Notes")
	org-mobile-directory (concat org-directory "/Mobile")
	org-mobile-inbox-for-pull (concat org-directory "/flagged.org") ;; TODO: ??

	org-refile-targets '((org-agenda-files :maxlevel . 1))
	org-outline-path-complete-in-steps nil
	org-refile-use-outline-path t
	org-agenda-files '("Personal.org" "Work.org" "Inbox.org")
	org-ellipsis " ▾"
	org-agenda-start-with-log-mode t
	org-log-done 'time
	org-log-into-drawer t
	org-return-follows-link t
	org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted"))
	
	org-agenda-custom-commands
	`(("p" "Planning"
	   ((tags-todo "+planning"
                       ((org-agenda-overriding-header "Planning Tasks")))
	    (tags-todo "-{.*}"
                       ((org-agenda-overriding-header "Untagged Tasks")))
	    (todo "*" ((org-agenda-files '(,(jd/org-mode-file "Inbox")))
		       (org-agenda-overriding-header "Unprocessed Inbox Items")))))

	  ("d" "Daily Agenda"
	   ((agenda "" ((org-agenda-span 'day)
			(org-deadline-warning-days 7)))
	    (tags-todo "+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "High Priority Tasks")))))

	  ("w" "Weekly Review"
	   ((agenda ""
		    ((org-agenda-overriding-header "Completed Tasks")
		     (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
		     (org-agenda-span 'week)))

	    (agenda ""
		    ((org-agenda-overriding-header "Unfinished Scheduled Tasks")
		     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		     (org-agenda-span 'week))))))
	
	org-capture-templates
	`(("i" "Capture to Inbox" entry (file+olp ,(jd/org-mode-file "Inbox") "Inbox")
	   "* TODO %?\n  %t\n" :empty-lines 1))
	
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  
  (require 'org-tempo)
  
  (defun jd/org-font-setup ()
    ;; Replace list hyphen with dot
    ;; (font-lock-add-keywords 'org-mode
    ;; 			    '(("^ *\\([.]\\) "
    ;; 			       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•-"))))))

    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.3)
		    (org-level-2 . 1.2)
		    (org-level-3 . 1.1)
		    (org-level-4 . 1.0)
		    (org-level-5 . 1.0)
		    (org-level-6 . 1.0)
		    (org-level-7 . 1.0)
		    (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil :font "Terminus" :weight 'Bold :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil    :font "Terminus" :inherit 'fixed-pitch :height 100)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

  (with-eval-after-load 'org (jd/org-font-setup))

  (defun jd-emacs/org-timer-stop ()
    (start-process-shell-command "notify-send" nil "notify-send Zakonczono odliczanie"))

  (add-hook 'org-timer-stop-hook #'jd-emacs/org-timer-stop)

  (defun jd/org-tempo-setup ()
    (dolist (template '(("s" . "src")
			("sql" . "src sql")
			("sh" . "src sh")
			("el" . "src emacs-lisp")
			("li" . "src lisp")
			("sc" . "src scheme")
			("ts" . "src typescript")
			("py" . "src python")
			("go" . "src go")
			("yaml" . "src yaml")))
      (add-to-list 'org-structure-template-alist template)))

  (with-eval-after-load 'org-tempo (jd/org-tempo-setup))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

(use-package org-superstar
  :guix-package "emacs-org-superstar"
  :hook (org-mode . org-superstar-mode)
  :init
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :guix-package "emacs-org-roam"
  :custom
  (org-roam-directory (file-truename jd/org-roam-home))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  
  (defun jd/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (defun jd/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
	    (seq-filter
	     (jd/org-roam-filter-by-tag tag-name)
	     (org-roam-node-list))))

  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :target (file+head
		    "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n\n* ${title}\n %?") ;; TODO: point cursor to the end of the file, it should be: `%?`
	   :unnarrowed t)
	  ;; ("n" "insert node" plain (file "~/Documents/roam/study/templates/research.org")
	  ;;  :target (file+head "study/%<%Y%m%d%H%M%S>-${slug}.org"
	  ;; 		      "#+title: ${title}\n")
	  ;;  :unnarrowed t)
	  ))

  (org-roam-db-autosync-mode))

(use-package ox-pandoc
  :guix-package "emacs-ox-pandoc")

(provide 'jd-org)

;;; jd-org.el ends here
