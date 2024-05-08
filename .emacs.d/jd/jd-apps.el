;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(use-package vterm
  :guix-package "emacs-vterm"
  :init
  (add-to-list 'project-switch-commands     '(project-vterm "Vterm") t)
  (add-to-list 'project-kill-buffer-conditions
	       '(major-mode . vterm-mode))
  (setq vterm-copy-exclude-prompt t)
  :config
  (setq vterm-buffer-name "vterm")
  ;; (evil-set-initial-state 'vterm-mode 'emacs)
  (setq vterm-tramp-shells
	(append  '(("ssh" "/bin/bash")) vterm-tramp-shells))

  )

(use-package all-the-icons-dired
  :guix-package "emacs-all-the-icons-dired"
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

(use-package pdf-tools
  :guix-package
  "emacs-pdf-tools")

(use-package password-store
  :guix-package "emacs-password-store"
  :bind
  ("C-c P p" . password-store-copy)
  ("C-c P i" . password-store-insert)
  ("C-c P g" . password-store-generate))

(use-package shell
  :bind
  ("C-c C-<return>" . shell))

(provide 'jd-apps)

;;; jd-apps.el ends here
