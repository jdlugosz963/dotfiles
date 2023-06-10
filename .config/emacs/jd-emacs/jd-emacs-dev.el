;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(setq c-default-style "linux"
      c-basic-offset 8)

(setq gdb-many-windows t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-diagnostics-provider :none)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ivy
  :after lsp)

(use-package paredit)
(use-package evil-paredit)
(use-package rainbow-delimiters)

(defun jd/lisp-mode-setup ()
  (rainbow-delimiters-mode)
  (evil-paredit-mode)
  (paredit-mode))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'scheme-mode-hook 'jd/lisp-mode-setup)
            (add-hook 'emacs-lisp-mode-hook 'jd/lisp-mode-setup)
            (add-hook 'clojure-mode-hook 'jd/lisp-mode-setup)
            (add-hook 'lisp-mode-hook 'jd/lisp-mode-setup)))


(evil-define-key 'normal paredit-mode-map (kbd "g h") 'paredit-forward-barf-sexp)
(evil-define-key 'normal paredit-mode-map (kbd "g l") 'paredit-forward-slurp-sexp)

(evil-define-key 'normal paredit-mode-map (kbd "g H") 'paredit-backward-slurp-sexp)
(evil-define-key 'normal paredit-mode-map (kbd "g L") 'paredit-backward-barf-sexp)

(use-package geiser)
(use-package geiser-guile)

(defun jd/python-mode-setup ()
  (let ((project-venv-path (concat (projectile-project-root) "venv/")))
    (when (projectile--directory-p project-venv-path)
      (pyvenv-activate project-venv-path)
      (pyvenv-mode))))

;;  (use-package python-mode ;;TODO: find alternative
;;    :hook (python-mode . lsp-deferred)
;;    :hook (python-mode . jd/python-mode-setup)
;;    :config
;;    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode)))

(use-package pyvenv
  :after python-mode)

(use-package typescript-mode
  :mode ("\\.ts\\'")
  :config
  (setq typescript-indent-level 2))

(defun jd/activate-tide-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.[tj]sx?\\'" buffer-file-name))
    (tide-setup)
    (tide-hl-identifier-mode)))

(use-package tide
  :after (typescript-mode company web-mode))

(use-package flycheck
  :hook ((after-init . global-flycheck-mode)))

(use-package web-mode
  :hook ((web-mode . jd/activate-tide-mode))
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

(use-package yaml-mode)

(use-package docker)

(jd/leader-key-def
  "d" '(:ignore t :which-key "Docker")
  "dc" '(docker-containers :which-key "Docker containers")
  "dd" '(docker :which-key "Docker"))

(use-package company
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

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :init
  (when (file-directory-p "~/Documents/code")
    (setq projectile-project-search-path '("~/Documents/code/")))
  :custom ((projectile-Completion-system 'ivy))
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode))

(jd/leader-key-def
  "p" '(projectile-command-map :which-key "Project")
  "p <ESC>" '()
  "SPC" '(projectile-find-file :which-key "Find file in project"))

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
      (message "Could not find git project root."))))

(use-package neotree
  :init
  (setq neo-theme 'icons)
  :config

  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "C-RET") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

(jd/leader-key-def
  "op" '(jd/neotree-project-dir :which-key "Open neotree"))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(jd/leader-key-def
  "g" '(:ignore t :which-key "Git")
  "gg" '(magit-status-here :which-key "Magit status"))

;;; jd-dev.el ends here
