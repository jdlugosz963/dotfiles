;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(defvar jd/org-home "~/Documents/Org")
(defvar jd/org-roam-home (concat jd/org-home "/roam"))
(defvar jd/org-roam-daily-home (concat jd/org-home "/roam/daily"))

(defun jd/org-mode-init ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun jd-emacs/org-insert-date (&optional date)
  (let ((d (or date "+0d")))
    (org-insert-time-stamp (org-read-date nil t d))))

(jd/use-package org "emacs-org"
		:pin org
		:commands (org-capture org-agenda)
		:hook (org-mode . jd/org-mode-init)
		:config
		(setq org-directory (file-truename "~/Documents/Org/"))
		(setq org-mobile-inbox-for-pull (concat org-directory "flagged.org"))
		(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
		(setq org-agenda-files
		      '("Tasks.org"))
		(setq org-ellipsis " ▾")
		(setq org-agenda-start-with-log-mode t)
		(setq org-log-done 'time)
		(setq org-log-into-drawer t)
		(setq org-return-follows-link t)
		(setq org-capture-templates
		      `(("t" "Tasks / Projects")
			("tt" "Task" entry (file+olp "Tasks.org" "Inbox")
			 "* TODO %?\n  %t\n  %a\n  %i" :empty-lines 1)
			("tT" "Task for tomorow" entry (file+olp "Tasks.org" "Inbox")
			 "* TODO %?\n %t\n  SCHEDULED: %(jd-emacs/org-insert-date \"+1d\")>\n %a\n %i" :empty-lines 1)

			("m" "Metrics Capture")
			("mm" "Metrics" table-line (file+headline "Metrics.org" "Metrics")
			 "| %U | %^{Weight} | %^{Waist} | %^{Notes} |" :kill-buffer t)
			("sh" "School Homework" entry (file+olp "school/todo(jd-emacs/org-insert-date \"1\").org"))))

		(setq org-latex-listings 'minted
		      org-latex-packages-alist '(("" "minted"))
		      org-latex-pdf-process
		      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
			"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
		(require 'org-tempo)
		
		(jd/leader-key-def
		 "o" '(:ignore t :which-key "Open/Org")
		 "oc" '(org-capture :which-key "Open org-capture")
		 "oop" '(org-mobile-pull :which-key "Org mobile pull")
		 "ooP" '(org-mobile-push :which-key "Org mobile push")
		 "oa" '(org-agenda :which-key "Open org-agenda"))

		(defun jd/org-font-setup ()
		  ;; Replace list hyphen with dot
		  (font-lock-add-keywords 'org-mode
					  '(("^ *\\([-]\\) "
					     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•-"))))))

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
		  (set-face-attribute 'org-table nil    :font "Terminus" :inherit '(shadow fixed-pitch))
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
		  (add-to-list 'org-structure-template-alist '("s" . "src"))
		  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
		  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
		  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
		  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
		  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
		  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
		  (add-to-list 'org-structure-template-alist '("py" . "src python"))
		  (add-to-list 'org-structure-template-alist '("go" . "src go"))
		  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml")))

		(with-eval-after-load 'org-tempo (jd/org-tempo-setup))

		(org-babel-do-load-languages
		 'org-babel-load-languages
		 '((emacs-lisp . t)
		   (python . t)))

		(defun jd/org-babel-tangle-config ()
		  (when (string-equal (file-name-directory (buffer-file-name))
				      (expand-file-name "~/dotfiles/"))
		    ;; Dynamic scoping to the rescue
		    (let ((org-confirm-babel-evaluate nil))
		      (org-babel-tangle))))

		(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jd/org-babel-tangle-config)))

		(defun jd/org-mode-visual-fill ()
		  (setq visual-fill-column-width 100
			visual-fill-column-center-text t)
		  (visual-fill-column-mode 1)))

;; (jd/use-package evil-org "emacs-evil-org"
;; 		:after org
;; 		:hook (org-mode . (lambda () evil-org-mode))
;; 		:config
;; 		(require 'evil-org-agenda)
;; 		(evil-org-agenda-set-keys))

(jd/use-package visual-fill-column "emacs-visual-fill-column"
		:hook (org-mode . jd/org-mode-visual-fill))

(jd/use-package org-superstar "emacs-org-superstar"
		:hook (org-mode . org-superstar-mode)
		:init
		(setq org-superstar-special-todo-items t)
		(setq org-superstar-remove-leading-stars t)
		(setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

; (jd/add-package-to-manifest "emacs-emacsql-sqlite3")
(jd/use-package org-roam "emacs-org-roam"
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
		(defun jd/org-roam-filter-by-tag (tag-name)
		  (lambda (node)
		    (member tag-name (org-roam-node-tags node))))

		(defun jd/org-roam-list-notes-by-tag (tag-name)
		  (mapcar #'org-roam-node-file
			  (seq-filter
			   (jd/org-roam-filter-by-tag tag-name)
			   (org-roam-node-list))))

		(defun jd/org-roam-refreshagenda-list ()
		  (interactive)
		  (setq org-agenda-files (org-roam-list-files)))

		(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
		(setq org-roam-capture-templates
		      '(("a" "workstuff" plain (file (concat org-roam-directory "/work"))
			 :target (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org"
					    "#+title: ${title}\n") :unnarrowed t)
			("b" "research" plain (file "~/Documents/roam/study/templates/research.org")
			 :target (file+head "study/%<%Y%m%d%H%M%S>-${slug}.org"
					    "#+title: ${title}\n") :unnarrowed t)
			("s" "School" plain nil
			 :target (file+head
				  "school/%<%Y%m%d%H%M%S>-${slug}.org"
				  "#+title: ${title}\n")
			 :unnarrowed t)
			("d" "default" plain nil
			 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
					    "#+title: ${title}\n") :unnarrowed t)
			))

		(org-roam-db-autosync-mode))

(provide 'jd-org)

;;; jd-org.el ends here
