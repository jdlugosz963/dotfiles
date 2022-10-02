(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")


(use-package org-msg
  :ensure t)

(use-package mu4e
  :defer 20
  :ensure nil
  :config
  (require 'org-mu4e)

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
                    ))
          ,(make-mu4e-context
            :name "Gmail"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Jakub Dlugosz")
                    (user-mail-address . "jdlugosz963@gmail.pl")
                    (mu4e-sent-folder . "/gmail/\[Gmail\]/Wys\&AUI-ane")
                    (mu4e-trash-folder . "/gmail/\[Gmail\]/Kosz")
                    (mu4e-drafts-folder . "/gmail/\[Gmail\]/Wersje\ robocze")
                    (mu4e-refile-folder . "/gmail/Archive")
                    (mu4e-sent-messages-behavior . sent)
                    ))))

  (setq mail-user-agent 'mu4e-user-agent
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.abaks.pl"
        smtpmail-local-domain "pl"
        smtpmail-smtp-service 465
        smtpmail-stream-type  'ssl)

  (mu4e t))

(jd/leader-key-def
  "m"  '(:ignore t :which-key "mail")
  "mm" 'mu4e
  "mc" 'mu4e-compose-new)


(provide 'jd-mu4e)
