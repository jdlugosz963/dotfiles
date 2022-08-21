(setq gc-cons-threshold (* 50 1000 1000))

(defun jd/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'jd/display-startup-time)

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

(require 'use-package)

(use-package general
  :config
  (general-create-definer jd/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer jd/ctrl-c-keys
    :prefix "C-c"))

(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(jd/leader-key-def
  "w"  'evil-window-map
  "wd" '(evil-window-delete :which-key "Window delete"))

(jd/leader-key-def
  "ou" '(undo-tree-visualize :which-key "Open"))

;; remove . key
(define-key evil-normal-state-map (kbd ".") '())

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

(set-face-attribute 'default nil :font "Hack" :height 100)

(add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package doom-themes
  :config
  (load-theme 'doom-molokai t))

(use-package diminish)

(use-package hl-todo
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

(use-package counsel)
(use-package ivy
  :diminish
  :bind
  (("C-s" . swiper)
   :map ivy-minibuffer-map
   ("C-k" . ivy-previous-line)
   ("C-j" . ivy-next-line)
   :map ivy-switch-buffer-map
  ("C-k" . ivy-previous-line))
  :config
  (ivy-mode 1))

(jd/leader-key-def
  "t"  '(:ignore t :which-key "Toogle")
  "tt" '(counsel-load-theme :which-key "Choose theme"))

(jd/leader-key-def
  "bb" '(jd/switch-buffer :which-key "Buffer switch")
  "ba" '(counsel-switch-buffer :which-key "Buffer switch")
  "b"  '(:ignore t :which-key "Buffer")
  "," '(counsel-switch-buffer :which-key "Buffer switch"))

(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package beacon
  :config
  (beacon-mode 1))

(jd/leader-key-def
  "f"  '(:ignore t :which-key "Files")
  "fs" '(save-buffer :which-key "File save")
  "." '(find-file :which-key "Find file")
  "ff" '(find-file :which-key "Find file"))

(jd/leader-key-def
  "e"   '(:ignore t :which-key "Eval")
  "eb"  '(eval-buffer :which-key "Eval buffer"))

(jd/leader-key-def
  :keymaps '(visual)
  "er" '(eval-region :which-key "Eval region"))

(defun jd/kill-other-buffers () 
  (interactive)                                                                   
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(jd/leader-key-def
  "bK" '(jd/kill-other-buffers :which-key "Kill other buffers"))
  "bk" '(kill-current-buffer :which-key "Kill buffer")
  "bB" '(ibuffer :which-key "Open ibuffer"))

(defun jd/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . jd/org-mode-setup)
  :init
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-return-follows-link t)
  :config
  (menu-bar--display-line-numbers-mode-none)
  (require 'org-tempo))

(jd/leader-key-def
  "o" '(:ignore t :which-key "Open")
  "oa" '(org-agenda :which-key "Open org-agenda"))

(use-package toc-org
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

(defun jd/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•-"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Monospace" :weight 'Bold :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :font "mononoki Nerd Font" :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(with-eval-after-load 'org-faces (jd/org-font-setup))

(defun jd/org-tempo-setup ()
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml")))

(with-eval-after-load 'org-tempo (jd/org-tempo-setup))

(defun jd/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/dotfiles/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jd/org-babel-tangle-config)))

(defun jd/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jd/org-mode-visual-fill))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :init
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

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

(defun jd/python-mode-setup ()
  (let ((project-venv-path (concat (projectile-project-root) "venv/")))
    (when (projectile--directory-p project-venv-path)
      (pyvenv-activate project-venv-path)
      (pyvenv-mode))))

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :hook (python-mode . jd/python-mode-setup))

(use-package pyvenv
  :after python-mode)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

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

(defun jd/open-new-vterm ()
  (interactive)
  (let ((buffer-name (generate-new-buffer-name vterm-buffer-name)))
    (switch-to-buffer buffer-name)
    (vterm-mode)))

(use-package vterm
  :config
  (setq vterm-buffer-name "vterm"))

(jd/leader-key-def
  "ot" '(jd/open-new-vterm :which-key "Open terminal in current window"))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-ranger)

(use-package dired-single)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "y" 'dired-ranger-copy
    "p" 'dired-ranger-paste
    "X" 'dired-ranger-move
    "h" 'dired-up-directory
    "l" 'dired-single-buffer))

(use-package emms
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-standard)
  (emms-default-players)
  (emms-mode-line-disable)
  (emms-add-directory-tree "~/Documents/music/"))
