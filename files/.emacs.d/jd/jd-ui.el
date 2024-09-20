;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(setq inhibit-startup-message t)
(setq visible-bell t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

;; (set-frame-parameter (selected-frame) 'alpha '(92 . 92))
;; (add-to-list 'default-frame-alist '(alpha . (92 . 92)))

(let ((height (if (string-equal (system-name)
				"berserker")
		  175
		125)))
 (custom-set-faces
  `(default ((t (:inherit nil :height ,height :family "Terminus"))))
  `(line-number ((t (:inherit nil :height ,height :family "Terminus"))))
  `(line-number-current-line ((t (:inherit nil :height ,height :family "Terminus"))))))

(add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-prettify-symbols-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'diminish)

(use-package doom-modeline
  :guix-package "emacs-doom-modeline"
  :config
  (doom-modeline-mode))

(use-package solarized-theme
  :guix-package "emacs-solarized-theme"
  :config
  (load-theme 'solarized-dark-high-contrast t))


(use-package diminish
  :guix-package "emacs-diminish")

(use-package hl-todo
  :guix-package "emacs-hl-todo"
  :init
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#FF0000")
	  ("FIXME"  . "#FF0000")
	  ("DEBUG"  . "#A020F0")))
  :config
  (global-hl-todo-mode 1))

(defvar jd/load-theme-hook nil)
(defun jd/load-theme ()
  (interactive)
  (counsel-load-theme)
  (run-hooks 'jd/load-theme-hook))

(use-package ivy
  :guix-package "emacs-ivy"
  :diminish
  :bind
  (("C-s" . swiper))
  :config
  (ivy-mode 1))

(use-package counsel
  :guix-package "emacs-counsel"
  :diminish t
  :config
  (counsel-mode 1))

(use-package which-key
  :guix-package "emacs-which-key"
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package all-the-icons
  :guix-package "emacs-all-the-icons")

(use-package beacon
  :guix-package "emacs-beacon"
  :config
  (beacon-mode 1))

(setq display-time-string-forms
      '(" " 24-hours ":" minutes " "))

(when jd/exwm-p
  (display-battery-mode)
  (display-time-mode))

(provide 'jd-ui)

;;; jd-ui.el ends here
