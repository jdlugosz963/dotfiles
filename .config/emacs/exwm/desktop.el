(defun jd/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun jd-exwm/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun jd-exwm/exwm-init-hook ()
  (display-battery-mode t)
  (display-time-mode t)

  (jd-exwm/run-in-background "nm-applet")
  (jd-exwm/run-in-background "pasystray")
  (jd-exwm/run-in-background "blueman-applet"))

(defun jd-exwm/exwm-update-title ()
  (exwm-workspace-rename-buffer exwm-title))

(defun jd-exwm/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-move-window 2))
    ("qutebrowser" (exwm-workspace-move-window 2))
    ("discord" (exwm-workspace-move-window 5))
    ("obs" (exwm-workspace-move-window 5))
    ("Virt-manager" (exwm-workspace-move-window 4))))

(use-package exwm
  :config
  (setq exwm-workspace-number 9)

  (add-hook 'exwm-init-hook #'jd-exwm/exwm-init-hook)
  (add-hook 'exwm-update-class-hook #'jd/exwm-update-class)
  (add-hook 'exwm-update-title-hook #'jd-exwm/exwm-update-title)
  (add-hook 'exwm-manage-finish-hook #'jd-exwm/configure-window-by-class)

  (start-process-shell-command "xmodmap" nil "xmodmap ~/.config/emacs/exwm/Xmodmap")
  (start-process-shell-command "nitrogen" nil "nitrogen --restore")
  (start-process-shell-command "xinput" nil "xinput set-prop 11 336 1")

  (require 'exwm-randr)
  (exwm-randr-enable)
  ;; (start-process-shell-command "xrandr" nil "xrandr --output DP-1-8 --primary --mode 1920x1080 --output eDP-1 --off")

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (setq exwm-input-simulation-keys
        '(([?\C-l] . [left])
          ([?\C-h] . [right])
          ([?\C-k] . [up])
          ([?\C-j] . [down])))

  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j 
          ?\C-\ ))  

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (exwm-input-set-key (kbd "s-p") 'counsel-linux-app)

  (setq exwm-input-global-keys
        `(([?\s-R] . exwm-reset)


          ([?\s-r] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ([?\s-W] . exwm-workspace-move-window)
          ([?\s-w] . exwm-workspace-switch)

          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 1 9))))

  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist `(alpha . (95 . 95)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
