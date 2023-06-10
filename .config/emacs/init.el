;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(defvar jd/exwmp (not (null (getenv "JD_EXWM")))
  "Return non-nil if Emacs is run as a window manager.")

(let ((jd-emacs-init (concat user-emacs-directory
			     "jd-emacs/jd-emacs-init.el"))
      (jd-exwm-init (concat user-emacs-directory
			    "jd-exwm/init.el")))
  (load jd-emacs-init)
  (when jd/exwmp
    (load jd-exwm-init)))

;;; init.el ends here
