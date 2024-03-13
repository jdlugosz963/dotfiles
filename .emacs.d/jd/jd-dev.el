;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(setq c-default-style "linux"
      c-basic-offset 8)

(setq gdb-many-windows t)

(jd/use-package lsp-mode "emacs-lsp-mode"
		:diminish t
		:commands (lsp lsp-deferred)
		:init
		(setq lsp-headerline-breadcrumb-enable nil)
		(setq lsp-diagnostics-provider :none)
		(setq lsp-modeline-diagnostics-enable nil)
		(setq lsp-keymap-prefix "C-c l")
		:config
		(lsp-enable-which-key-integration t))

(jd/use-package lsp-ivy "emacs-lsp-ivy"
		:after lsp)

(jd/use-package paredit "emacs-paredit")

(defun jd/lisp-mode-setup ()
  (rainbow-delimiters-mode)
  (paredit-mode))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (add-hook 'scheme-mode-hook 'jd/lisp-mode-setup)
	    (add-hook 'emacs-lisp-mode-hook 'jd/lisp-mode-setup)
	    (add-hook 'clojure-mode-hook 'jd/lisp-mode-setup)
	    (add-hook 'lisp-mode-hook 'jd/lisp-mode-setup)))

(jd/use-package rainbow-delimiters "emacs-rainbow-delimiters")

(jd/use-package sly "emacs-sly")

(jd/use-package geiser "emacs-geiser")
(jd/use-package geiser-racket "emacs-geiser-racket")
(jd/use-package racket-mode "emacs-racket-mode")
(jd/use-package geiser-guile "emacs-geiser-guile"
		:config
		(when jd/guix-p
		  (defun jd/guix-repl ()
		    (interactive)
		    (let ((geiser-guile-binary '("guix" "repl"))
			  (geiser-guile-load-path (cons "~/dotfiles/guix" geiser-guile-load-path)))
		      (geiser 'guile)))))

(jd/use-package pyvenv "emacs-pyvenv"
		:after python-mode)

(jd/use-package typescript-mode "emacs-typescript-mode"
		:mode ("\\.ts\\'")
		:config
		(setq typescript-indent-level 2))

(jd/use-package cider "emacs-cider")

(jd/use-package tide "emacs-tide"
		:after (typescript-mode company web-mode))

(jd/use-package flycheck "emacs-flycheck"
		:hook ((after-init . global-flycheck-mode)))

(jd/use-package web-mode "emacs-web-mode"
		;; :hook ((web-mode . jd/activate-tide-mode))
		:mode
		("\\.ejs\\'" "\\.hbs\\'" "\\.html\\'" "\\.php\\'" "\\.[jt]sx?\\'")
		:config
		(setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")))
		(setq web-mode-markup-indent-offset 2)
		(setq web-mode-css-indent-offset 2)
		(setq web-mode-code-indent-offset 2)
		(setq web-mode-script-padding 2)
		(setq web-mode-block-padding 2)
		(setq web-mode-style-padding 2)
		(setq web-mode-enable-auto-pairing t)
		(setq web-mode-enable-auto-closing t)
		(setq web-mode-enable-current-element-highlight t))

(jd/use-package yaml-mode "emacs-yaml-mode")

(jd/use-package docker "emacs-docker"
		:bind
		("C-c D d" . docker-containers)
		("C-c D D" . docker)
		:config
		(jd/leader-key-def
		  "d" '(:ignore t :which-key "Docker")
		  "dc" '(docker-containers :which-key "Docker containers")
		  "dd" '(docker :which-key "Docker")))

(jd/use-package company "emacs-company"
		:diminish t
		:after lsp-mode
		:hook (lsp-mode . company-mode)
		:bind (:map company-active-map
			    ("<tab>" . company-complete-selection))
		(:map lsp-mode-map
		      ("<tab>" . company-indent-or-complete-common))
		:custom
		(company-minimum-prefix-length 1)
		(company-idle-delay 0.0)
		:config
		(global-company-mode))

;; (jd/use-package company-box "emacs-company-box"
;; 		:diminish t
;; 		:hook (company-mode . company-box-mode))

(jd/use-package projectile "emacs-projectile"
		:diminish projectile-mode
		:init
		(when (file-directory-p "~/Documents/code")
		  (setq projectile-project-search-path '("~/Documents/code/")))
		:custom ((projectile-Completion-system 'ivy))
		:config
		(setq projectile-switch-project-action #'projectile-dired)
		(projectile-mode)

		(jd/leader-key-def
		  "p" '(projectile-command-map :which-key "Project")
		  "p <ESC>" '()
		  "SPC" '(projectile-find-file :which-key "Find file in project")))

(jd/use-package neotree "emacs-neotree"
		:init
		(setq neo-theme 'icons)
		:config
		(jd/leader-key-def
		  "op" '(jd/neotree-project-dir :which-key "Open neotree"))

		(defun jd/neotree-project-dir ()
		  "Open NeoTree using the git root."
		  (interactive)
		  (let ((project-dir (projectile-project-root))
			(file-name (buffer-file-name)))
		    (neotree-toggle)
		    (if project-dir
			(if (neo-global--window-exists-p)
			    (progn
			      (neotree-dir project-dir)
			      (neotree-find file-name)))
		      (message "Could not find git project root.")))))

(jd/use-package magit "emacs-magit"
		:custom
		(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(jd/use-package restclient "emacs-restclient")

(setq sql-connection-alist
      '(("net47-abaks"
	 (sql-product 'postgres)
	 (sql-user "kuba")
	 (sql-database "net47")
	 (sql-server "net47.abaks.pl"))))

(provide 'jd-dev)

;;; jd-dev.el ends here
