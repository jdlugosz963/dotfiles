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

(defun jd-emacs/erc-notify-sound ()
    (jd-exwm/run-in-background "mpv /home/jakub/.config/emacs/resources/beep.wav"))

;; (add-hook 'erc-notifications-mode-hook #'jd-emacs/erc-notify-sound)

(setq
 erc-nick "bobbma"
 erc-user-full-name "Jakub Dlugosz"
 erc-notify-list '("akuleszaa"))

;; Define a function to connect to a server
(defun hipis-znc ()
  (interactive)
  (erc :server "195.74.91.18"
       :port   "6697"))

;;; jd-custom.el ends here
