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

(provide 'jd-misc)

;;; jd-misc.el ends here
