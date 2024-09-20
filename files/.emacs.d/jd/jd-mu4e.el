;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(use-package htmlize
  :guix-package "emacs-htmlize")

(use-package mu4e
  :guix-package  ("mu" "isync")
  :defer 10
  :ensure nil
  :bind
  ("C-c M" . mu4e)
  :config
					;a  (require 'org-mu4e)

  (unless jd/guix-p
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
    (setq mu4e-mu-binary "/usr/local/bin/mu"))

  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  (setq mu4e-completing-read-function #'ivy-completing-read)

  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-contexts
	`(,(make-mu4e-context
	    :name "Abaks"
	    :match-func (lambda (msg) (when msg
				   (string-prefix-p "/abaks" (mu4e-message-field msg :maildir))))
	    :vars '(
		    (user-full-name . "Jakub Dlugosz")
		    (user-mail-address . "jakub@abaks.pl")
		    (mu4e-sent-folder . "/abaks/Sent Items")
		    (mu4e-trash-folder . "/abaks/Trash")
		    (mu4e-drafts-folder . "/abaks/Drafts")
		    (mu4e-refile-folder . "/abaks/Archive")
		    (mu4e-sent-messages-behavior . sent)
		    (smtpmail-local-domain . "pl")
		    (smtpmail-smtp-server . "smtp.abaks.pl")
		    (smtpmail-smtp-user . "jakub@abaks.pl")))
	  ,(make-mu4e-context
	    :name "Gmail"
	    :match-func (lambda (msg) (when msg
				   (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
	    :vars '(
		    (user-full-name . "Jakub Dlugosz")
		    (user-mail-address . "jdlugosz963@gmail.com")
		    (mu4e-sent-folder . "/gmail/\[Gmail\]/Wys\&AUI-ane")
		    (mu4e-trash-folder . "/gmail/\[Gmail\]/Kosz")
		    (mu4e-drafts-folder . "/gmail/\[Gmail\]/Wersje\ robocze")
		    (mu4e-refile-folder . "/gmail/Archive")
		    (smtpmail-local-domain . "com")
		    (smtpmail-smtp-server . "smtp.gmail.com")
		    (smtpmail-smtp-user . "jdlugosz963@gmail.com")
		    (mu4e-sent-messages-behavior . sent)))))

  (setq mail-user-agent 'mu4e-user-agent
	mail-host-address nil
	message-send-mail-function 'smtpmail-send-it
	smtpmail-smtp-service 465
	smtpmail-stream-type  'ssl
	smtpmail-servers-requiring-authorization ".*")

  (setq mu4e-compose-signature (concat
				"Pozdrawiam,\n"
				"Jakub Długosz"))

  (mu4e t)
  (mu4e-modeline-mode nil))

(use-package mu4e-alert
  :guix-package "emacs-mu4e-alert"
  :defer 20
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))

(provide 'jd-mu4e)
