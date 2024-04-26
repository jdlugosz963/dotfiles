;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(defun jd/generete-qr-from-clipboard ()
  (interactive)
  (let ((clipboard-value (x-get-clipboard))
	(clipboard-file-path "/tmp/clipboard_value.txt")
	(clipboard-out-image "/tmp/qr.png"))
    (with-temp-file clipboard-file-path
      (insert clipboard-value))
    (shell-command (concat
		    "qrencode -o "
		    clipboard-out-image
		    " < "
		    clipboard-file-path))
    (find-file clipboard-out-image)))

(defun jd/dired-open-file-in-kill-ring ()
  (interactive)
  (let* ((last-killed (car kill-ring))
	 (exists-p (file-exists-p last-killed))
	 (dir-p (file-directory-p last-killed)))
    (cond
     ((not exists-p) (message "File doesn't exists!"))
     (dir-p (dired last-killed))
     ((not dir-p) (find-file last-killed)))))

(global-set-key (kbd "C-c O") #'jd/dired-open-file-in-kill-ring)


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
