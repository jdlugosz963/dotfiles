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

(setq erc-nick "bobbma"
      erc-user-full-name "Jakub Dlugosz"
      erc-notify-list '("akuleszaa"))

(defun hipis-znc ()
  (interactive)
  (erc :server "195.74.91.18"
       :port   "6697"))

(provide 'jd-custom)

;;; jd-custom.el ends here
