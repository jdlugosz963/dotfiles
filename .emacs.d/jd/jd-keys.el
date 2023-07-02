;;; Dotfiles --- Jakub Dlugosz emacs config
;;; Commentary:

;;; Code:

;; (jd/use-package evil "emacs-evil"
;; 		:init
;; 		(setq evil-want-integration t)
;; 		(setq evil-want-keybinding nil)
;; 		(setq evil-want-C-u-scroll t)
;; 		(setq evil-want-Y-yank-to-eol t) 
;; 		(setq evil-want-C-i-jump nil) 
;; 		(setq evil-undo-system 'undo-tree)
;; 		(setq evil-kill-on-visual-paste nil)
;; 		:config
;; 		(evil-set-initial-state 'exwm-mode 'emacs)
;; 		(evil-mode 1))

(jd/use-package general "emacs-general"
		:config
		(general-create-definer jd/leader-key-def
		  :keymaps '(normal insert visual emacs)
		  :prefix "SPC"
		  :global-prefix "C-z")

		(general-create-definer jd/ctrl-c-keys
		  :prefix "C-c")

		;; (jd/leader-key-def
		;;  "w"  'evil-window-map
		;;  "wr" '(hydra-resize-window/body :which-key "Rezize window")
		;;  "wd" '(evil-window-delete :which-key "Window delete"))
 
		(jd/leader-key-def
		 "ou" '(undo-tree-visualize :which-key "Open"))

		;; remove . key
		;; (define-key evil-normal-state-map (kbd ".") '())
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
		 "bB" '(ibuffer :which-key "Open ibuffer")))


(jd/use-package hydra "emacs-hydra"
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

		;; (defhydra hydra-resize-window (:timeout 4)
		;;   ("h" evil-window-decrease-width "<")
		;;   ("l" evil-window-increase-width ">")
		;;   ("k" evil-window-decrease-height "^")
		;;   ("j" evil-window-increase-height "v")
		;;   ("q" nil "finished" :exit t))

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
		  "ts" '(hydra-text-scale/body :which-key "Scale text")))


(jd/use-package undo-tree "emacs-undo-tree"
		:diminish
		:config
		(setq undo-tree-auto-save-history nil)
		(global-undo-tree-mode 1))

;; (jd/use-package evil-collection "emacs-evil-collection"
;; 		:after evil
;; 		:config
;; 		(evil-collection-init))


(provide 'jd-keys)

;;; jd-keys.el ends here
