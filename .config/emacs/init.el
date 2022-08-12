(setq user-full-name "Jakub Dlugosz"
      user-mail-address "jdlugosz963@gmail.com")

(setq gc-cons-threshold (* 50 1000 1000))

(defun jd/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'jd/display-startup-time)

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

                                        ; (load-theme 'wombat)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(use-package diminish)

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
  "bb" '(counsel-switch-buffer :which-key "Buffer switch")
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

(jd/leader-key-def
  "bk" '(kill-this-buffer :which-key "Buffer kill"))

(defun jd/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . jd/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (require 'org-tempo))

(jd/leader-key-def
  "o" '(:ignore t :which-key "Open")
  "oa" '(org-agenda :which-key "Open org-agenda"))

(use-package org-make-toc)

(add-hook 'org-mode-hook #'org-make-toc-mode)
; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-make-toc)))

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
                      (expand-file-name user-emacs-directory))
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
  :config
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package projectile
  :diminish projectile-mode
  :init
  (when (file-directory-p "~/Documents/code")
    (setq projectile-project-search-path '("~/Documents/code/")))
  :config (projectile-mode)
  :custom ((projectile-Completion-system 'ivy)))

(jd/leader-key-def
  "p" '(projectile-command-map :which-key "Project")
  "p <ESC>" '()
  "SPC" '(projectile-find-file :which-key "Find file in project"))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(jd/leader-key-def
  "g" '(:ignore t :which-key "Git")
  "gg" '(magit-status-here :which-key "Magit status"))

(use-package vterm)
(use-package vterm-toggle
  :init
  (setq vterm-toggle-fullscreen-p nil)
  :config
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(jd/leader-key-def
  "oT" '(vterm :which-key "Open terminal in current window")
  "ot" '(vterm-toggle :which-key "Toggle terminal"))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-ranger)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-single-buffer))
