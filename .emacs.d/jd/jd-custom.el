;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(setq erc-nick "bobbma"
      erc-user-full-name "Jakub Dlugosz"
      erc-notify-list '("akuleszaa"))

(defun hipis-znc ()
  (interactive)
  (erc :server "195.74.91.18"
       :port   "6697"))


;; Repair load paths when tramp try to connect to guix instances
(require 'tramp)

(connection-local-set-profile-variables
 'guix-system
 '((tramp-remote-path . (tramp-own-remote-path))))

(connection-local-set-profiles
 '(:application tramp :protocol "sudo" :machine "localhost")
 'guix-system)

(connection-local-set-profiles
 '(:application tramp :protocol "ssh" :machine "jdlugosz.com")
 'guix-system)

(provide 'jd-custom)

;;; jd-custom.el ends here
