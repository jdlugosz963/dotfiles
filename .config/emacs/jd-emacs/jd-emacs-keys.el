;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

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
  (setq evil-want-Y-yank-to-eol t) 
  (setq evil-want-C-i-jump nil) 
  (setq evil-undo-system 'undo-tree)
  (setq evil-kill-on-visual-paste nil)
  :config
  (evil-set-initial-state 'exwm-mode 'emacs)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(jd/leader-key-def
  "w"  'evil-window-map
  "wr" '(hydra-resize-window/body :which-key "Rezize window")
  "wd" '(evil-window-delete :which-key "Window delete"))

(jd/leader-key-def
  "ou" '(undo-tree-visualize :which-key "Open"))

;; remove . key
(define-key evil-normal-state-map (kbd ".") '())

;;; jd-keys.el ends here
