;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

(setq inhibit-startup-message t)
(setq visible-bell t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

(set-face-attribute 'default nil :font "Terminus" :height 100)

(add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-prettify-symbols-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package doom-themes
  :config
  (load-theme 'ujelly t))

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

(defvar jd/load-theme-hook nil)
(defun jd/load-theme ()
  (interactive)
  (counsel-load-theme)
  (run-hooks 'jd/load-theme-hook))

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

(use-package counsel
  :config
  (counsel-mode 1))

(jd/leader-key-def
  "t"  '(:ignore t :which-key "Toggle")
  "tT" '(toggle-truncate-lines :which-key "Toggle truncate lines")
  "tt" '(jd/load-theme  :which-key "Choose theme"))

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
  ; :init (doom-modeline-mode 0)
  :custom ((doom-modeline-height 15)))

(use-package hydra
  :defer t)

(defun jd/text-scale-increase ()
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

(defun jd/text-scale-decrease ()
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))))

(defhydra hydra-resize-window (:timeout 4)
  ("h" evil-window-decrease-width "<")
  ("l" evil-window-increase-width ">")
  ("k" evil-window-decrease-height "^")
  ("j" evil-window-increase-height "v")
  ("q" nil "finished" :exit t))

(defhydra hydra-text-scale-global (:timeout 4)
  "scale text"
  ("j" jd/text-scale-increase "in")
  ("k" jd/text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(jd/leader-key-def
  "tS" '(hydra-text-scale-global/body :which-key "Scale text")
  "ts" '(hydra-text-scale/body :which-key "Scale text"))

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
  "eb"  '(eval-buffer :which-key "Eval buffer")
  "ee" '(eval-defun :which-key "Eval defun"))

(jd/leader-key-def
  :keymaps '(visual)
  "er" '(eval-region :which-key "Eval region"))

(defun jd/kill-other-buffers () 
  (interactive)                                                                   
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(jd/leader-key-def
  "bK" '(jd/kill-other-buffers :which-key "Kill other buffers")
  "bk" '(kill-current-buffer :which-key "Kill buffer")
  "bB" '(ibuffer :which-key "Open ibuffer"))

;;; jd-ui.el ends here
