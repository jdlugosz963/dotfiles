;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(setq gc-cons-threshold (* 50 1000 1000))

(defvar jd/manifest-list
  nil
  "List that contain Emacs packages for GNU/Guix." )

(defvar jd/exwm-p
  (or (not (null (getenv "JD_EXWM")))
      (and (seq-contains command-line-args "exwm" #'string-match)
	   t))
  "Is non-nil when Emacs is run as a window manager.")

(defvar jd/guix-p
  (and (eq system-type 'gnu/linux)
       (file-exists-p "/etc/os-release")
       (with-temp-buffer
         (insert-file-contents "/etc/os-release")
         (search-forward "ID=guix" nil t))
       t)
  "Is non-nil when Emacs packages are installed by GNU/Guix package manager.")

(defun jd/add-package-to-manifest (guix-package-name)
  "Add GUIX-PACKAGE-NAME to jd/manifest-list."
  (unless (member guix-package-name jd/manifest-list)
    (setq jd/manifest-list
	(cons guix-package-name jd/manifest-list))))

(add-to-list 'load-path (concat user-emacs-directory "/jd"))

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

(add-hook 'emacs-startup-hook #'jd-emacs/startup)

(setq user-full-name "Jakub Dlugosz"
      user-mail-address "jdlugosz963@gmail.com")

(setq backup-directory-alist '(("." . "~/.cache/emacs/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

;; (setq user-emacs-directory "~/.cache/emacs")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(jd/add-package-to-manifest "emacs-use-package")
(unless jd/guix-p
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))

(require 'use-package)

(setq use-package-always-ensure (not jd/guix-p))

(defmacro jd/use-package (package-name
			  guix-package-name
			  &rest body)
  `(progn
     ,(when guix-package-name
	`(jd/add-package-to-manifest ,guix-package-name))
     (use-package ,package-name ,@body)))

(defun jd/manifest-generate-to-file (file-name)
  "Generate GNU/Guix Emacs manifest to output file provided in: FILE-NAME."
  (interactive
   (list (read-from-minibuffer "Output file: " )))
  (with-temp-file file-name
    (insert
     (with-temp-buffer
       (insert (format ";; This file is auto-generated by Emacs function: jd/manifest-generate-to-file\n%s"
		       `(specifications->manifest
			 '(,@(mapcar #'(lambda (guix-package-name)
					 (concat "\""
						 guix-package-name
						 "\"\n"))
				     jd/manifest-list)))))
       (pp-buffer)
       (buffer-string)))))

(jd/add-package-to-manifest "emacs")
(jd/add-package-to-manifest "emacs-guix")

(require 'jd-keys)
(require 'jd-ui)
(require 'jd-org)
(require 'jd-dev)
(require 'jd-apps)
(require 'jd-custom)
(require 'jd-mu4e)

(when jd/exwm-p
  (require 'jd-exwm))

(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
