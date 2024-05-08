;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(setq c-default-style "linux"
      c-basic-offset 8
      gdb-many-windows t)

(use-package lsp-mode
  :guix-package "emacs-lsp-mode"
  :diminish t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-diagnostics-provider :none)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ivy
  :guix-package "emacs-lsp-ivy"
  :after lsp)


(use-package paredit
  :guix-package "emacs-paredit")

(defun jd/lisp-mode-setup ()
  (rainbow-delimiters-mode)
  (paredit-mode))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (add-hook 'scheme-mode-hook 'jd/lisp-mode-setup)
	    (add-hook 'emacs-lisp-mode-hook 'jd/lisp-mode-setup)
	    (add-hook 'clojure-mode-hook 'jd/lisp-mode-setup)
	    (add-hook 'lisp-mode-hook 'jd/lisp-mode-setup)))

(use-package rainbow-delimiters
  :guix-package "emacs-rainbow-delimiters")

(use-package sly
  :guix-package "emacs-sly")

(use-package geiser
  :guix-package "emacs-geiser")

(use-package geiser-racket
  :guix-package "emacs-geiser-racket")

(use-package racket-mode
  :guix-package "emacs-racket-mode")

(use-package geiser-guile
  :guix-package "emacs-geiser-guile"
  :config
  (when jd/guix-p
    (defun jd/guix-repl ()
      (interactive)
      (let ((geiser-guile-binary '("guix" "repl"))
	    (geiser-guile-load-path (cons "~/dotfiles/guix" geiser-guile-load-path)))
	(geiser 'guile)))))

(use-package pyvenv
  :guix-package "emacs-pyvenv"
  :after python-mode)

(use-package typescript-mode
  :guix-package "emacs-typescript-mode"
  :mode ("\\.ts\\'")
  :config
  (setq typescript-indent-level 2))

(use-package cider
  :guix-package "emacs-cider")

(use-package tide
  :guix-package "emacs-tide"
  :after (typescript-mode company web-mode))

(use-package flycheck
  :guix-package "emacs-flycheck"
  :hook ((after-init . global-flycheck-mode)))

(use-package web-mode
  :guix-package "emacs-web-mode"
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

(use-package yaml-mode
  :guix-package "emacs-yaml-mode")

(use-package docker
  :guix-package "emacs-docker"
  :bind
  ("C-c D d" . docker-containers)
  ("C-c D D" . docker))

(use-package company
  :guix-package "emacs-company"
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

(use-package projectile
  :guix-package "emacs-projectile"
  :diminish projectile-mode
  :init
  (when (file-directory-p "~/Documents/code")
    (setq projectile-project-search-path '("~/Documents/code/")))
  :custom ((projectile-Completion-system 'ivy))
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode))

(use-package magit
  :guix-package "emacs-magit"
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package restclient
  :guix-package "emacs-restclient")

(setq sql-connection-alist
      '(("net47-abaks"
	 (sql-product 'postgres)
	 (sql-user "kuba")
	 (sql-database "net47")
	 (sql-server "net47.abaks.pl"))))

(provide 'jd-dev)

;;; jd-dev.el ends here
