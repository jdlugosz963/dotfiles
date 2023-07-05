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

(jd/add-package-to-manifest "font-terminus")
(set-face-attribute 'default nil :font "Terminus" :height 100)

(add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-prettify-symbols-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(jd/use-package doom-themes "emacs-doom-themes"
		:config
		;; (load-theme 'manoj-black t)
		)

(load-theme 'manoj-dark t)

(jd/use-package diminish "emacs-diminish")

(jd/use-package hl-todo "emacs-hl-todo"
		:init
		(setq hl-todo-keyword-faces
		      '(("TODO"   . "#FF0000")
			("FIXME"  . "#FF0000")
			("DEBUG"  . "#A020F0")))
		:config
		(global-hl-todo-mode 1))

(defun jd/switch-buffer ()
  (interactive)
  (let ((completion-regexp-list '("\\`[^*]"
                                  "\\`\\([^T]\\|T\\($\\|[^A]\\|A\\($\\|[^G]\\|G\\($\\|[^S]\\|S.\\)\\)\\)\\).*")))
    (call-interactively 'counsel-switch-buffer)))

(defvar jd/load-theme-hook nil)
(defun jd/load-theme ()
  (interactive)
  (counsel-load-theme)
  (run-hooks 'jd/load-theme-hook))

(jd/use-package ivy "emacs-ivy"
		:diminish
		:bind
		(("C-s" . swiper)
		 ;; :map ivy-minibuffer-map
		 ;; ("C-k" . ivy-previous-line)
		 ;; ("C-j" . ivy-next-line)
		 ;; :map ivy-switch-buffer-map
		 ;; ("C-k" . ivy-previous-line)
		 )
		:config
		(ivy-mode 1))

(jd/use-package counsel "emacs-counsel"
		:config
		(counsel-mode 1)

		(jd/leader-key-def
		 "t"  '(:ignore t :which-key "Toggle")
		 "tT" '(toggle-truncate-lines :which-key "Toggle truncate lines")
		 "tt" '(jd/load-theme  :which-key "Choose theme"))

		(jd/leader-key-def
		 "bb" '(jd/switch-buffer :which-key "Buffer switch")
		 "ba" '(counsel-switch-buffer :which-key "Buffer switch")
		 "b"  '(:ignore t :which-key "Buffer")
		 "," '(counsel-switch-buffer :which-key "Buffer switch")))

(jd/use-package which-key "emacs-which-key"
		:diminish
		:config
		(which-key-mode)
		(setq which-key-idle-delay 0.3))

(jd/use-package all-the-icons "emacs-all-the-icons")

(jd/use-package doom-modeline "emacs-doom-modeline"
					; :init (doom-modeline-mode 0)
		:custom ((doom-modeline-height 15)))

(jd/use-package beacon "emacs-beacon"
		:config
		(beacon-mode 1))

		
(provide 'jd-ui)

;;; jd-ui.el ends here
