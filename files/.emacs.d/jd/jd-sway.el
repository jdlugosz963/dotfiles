;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(defun use-package-normalize/:diminish (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-diminish name) t))

(use-package shackle
  :guix-package "emacs-shackle")

(use-package sway
  :guix-package "emacs-sway")

(provide 'jd-sway)

;;; jd-sway.el ends here
