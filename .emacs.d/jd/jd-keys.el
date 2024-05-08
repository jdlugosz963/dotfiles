;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:
;; Custom keysets used in my emacs
;;; Code:

(use-package multiple-cursors
  :guix-package "emacs-multiple-cursors"
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))


(use-package hydra
  :guix-package "emacs-hydra"
  ;; :defer t
  :config
  (defun jd/text-scale-increase ()
    (interactive)
    (let ((old-face-attribute (face-attribute 'default :height)))
      (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

  (defun jd/text-scale-decrease ()
    (interactive)
    (let ((old-face-attribute (face-attribute 'default :height)))
      (set-face-attribute 'default nil :height (- old-face-attribute 10))))

  (defhydra hydra-text-scale-global (:timeout 4)
    "scale text"
    ("j" jd/text-scale-increase "in")
    ("k" jd/text-scale-decrease "out")
    ("q" nil "finished" :exit t)) ;; TODO not working

  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("q" nil "finished" :exit t)))

(use-package undo-tree
  :guix-package "emacs-undo-tree"
  :diminish
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(provide 'jd-keys)

;;; jd-keys.el ends here
