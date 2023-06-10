;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(defvar jd-emacs/directory
  (concat user-emacs-directory
	  "/jd-emacs/")
  "Path to jd-emacs config directory.")

(defun jd-emacs/load (file-name)
  "Load file from jd-emacs directory.
FILE-NAME - file name to load."
  (let ((file (expand-file-name (concat jd-emacs/directory
		      "jd-emacs-" file-name ".el"))))
    (load file)))

(setq gc-cons-threshold (* 2 1000 1000))


(defun jd/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

  
(defun jd-emacs/startup ()
  "This procedure will run after Emacs startup."
  (electric-pair-mode 1)
  (global-company-mode 1)
  (jd/display-startup-time))

(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook #'jd-emacs/startup)

(setq user-full-name "Jakub Dlugosz"
      user-mail-address "jdlugosz963@gmail.com")

(setq backup-directory-alist '(("." . "~/.cache/emacs/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(setq user-emacs-directory "~/.cache/emacs")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "~/.config/emacs/jd/"))

(require 'use-package)


(jd-emacs/load "keys")
(jd-emacs/load "ui")
(jd-emacs/load "org-mode")
(jd-emacs/load "dev")
(jd-emacs/load "apps")
(jd-emacs/load "custom")


(setq gc-cons-threshold (* 2 1000 1000))


;; jd-int.el ends here
