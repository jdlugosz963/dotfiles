;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

;; (defun jd/open-new-vterm (&optional jd/vterm-buffer-name)
;;   (interactive)
;;   (let ((buffer-name (generate-new-buffer-name vterm-buffer-name)))
;;     (when jd/vterm-buffer-name
;;       (setq buffer-name jd/vterm-buffer-name))
;;     (switch-to-buffer buffer-name)
;;     (vterm-mode)))


(jd/use-package vterm "emacs-vterm"
		:init
		(add-to-list 'project-switch-commands     '(project-vterm "Vterm") t)
		(add-to-list 'project-kill-buffer-conditions  '(major-mode . vterm-mode))
		(setq vterm-copy-exclude-prompt t)
		:config
		(setq vterm-buffer-name "vterm")
		;; (evil-set-initial-state 'vterm-mode 'emacs)
		(setq vterm-tramp-shells (append  '(("ssh" "/bin/bash")) vterm-tramp-shells))

		(jd/leader-key-def  "ot" '(jd/open-new-vterm :which-key "Open terminal in current window")))

(jd/use-package all-the-icons-dired "emacs-all-the-icons-dired"
		:hook (dired-mode . all-the-icons-dired-mode))

;; (jd/use-package dired-ranger nil)

(jd/use-package dired nil
		:ensure nil
		:commands (dired dired-jump)
		:custom ((dired-listing-switches "-agho --group-directories-first"))
		:config
		;; (evil-collection-define-key 'normal 'dired-mode-map
		;;   "y" 'dired-ranger-copy
		;;   "p" 'dired-ranger-paste
		;;   "X" 'dired-ranger-move
		;;   "h" 'dired-up-directory
		;;   "t" 'dired-create-empty-file
		;;   "T" 'dired-toggle-marks
		;;   "l" 'dired-find-file)
		(setq dired-kill-when-opening-new-dired-buffer t)
		;; (evil-define-key 'normal dired-mode-map (kbd "q") 'kill-current-buffer)
		)

(jd/use-package emms "emacs-emms"
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-standard)
  (emms-default-players)
  (emms-mode-line-disable)
  (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
  (emms-add-directory-tree "~/Documents/Music/"))

(jd/use-package pdf-tools "emacs-pdf-tools")

(jd/use-package elfeed "emacs-elfeed"
		:config
		(setq elfeed-feeds
		      '(("https://www.reddit.com/r/emacs.rss" emacs)
			("https://www.reddit.com/r/gnu.rss" gnu)
			("https://stallman.org/rss/rss.xml" stallman)
			("https://news.ycombinator.com/rss" hacker-news))))

;; (jd/use-package langtool
;;   :config
;;   (setq langtool-language-tool-jar "/home/jakub/Documents/LanguageTool-6.0/languagetool-commandline.jar")
;;   (setq langtool-default-language "pl-PL"))

;; (jd/leader-key-def
;;   "l"  '(:ignore t :which-key "langtool")
;;   "lc" 'langtool-check-buffer
;;   "ld" 'langtool-check-done
;;   "li" 'langtool-interactive-correction)

(jd/use-package mastodon "emacs-mastodon"
		:config
		(setq mastodon-active-user "jdlugosz963"
		      mastodon-instance-url "https://fosstodon.org/"))

(jd/use-package password-store "emacs-password-store"
		:bind
		("C-c P p" . password-store-copy)
		("C-c P i" . password-store-insert)
		("C-c P g" . password-store-generate))

(jd/use-package bluetooth "emacs-bluetooth"
		:bind
		("C-c B" . bluetooth-list-devices))

(jd/use-package shell nil
		:bind
		("C-c C-<return>" . shell))

(jd/use-package nov-el "emacs-nov-el")

(provide 'jd-apps)

;;; jd-apps.el ends here
