(in-package :stumpwm)


(setq *message-window-gravity* :center)
(setq *message-window-input-gravity* :center)
(setq *input-window-gravity* :center)
(setq *window-border-style* :none)

(set-bg-color :gray10)
(setq *mode-line-background-color* "Gray10")

(set-prefix-key (kbd "C-z"))
(setq *float-window-modifier* :super)

(define-key *top-map* (kbd "s-w") "gselect")

(defcommand lock () ()
  (run-shell-command "slock"))

(define-key *top-map* (kbd "s-l") "lock")

(defcommand server-start () ()
  (slynk:create-server :port 4005)
  (message "server started at port: \"4005\"!"))


(define-key *root-map* (kbd "C-q") "send-raw-key")

(define-key *root-map* (kbd "M") "mode-line")

;; (run-commands "gnew Code"
;; 	      "gnew Web"
;; 	      "gnew Music"
;; 	      "gnew Game"
;; 	      "gnew School")

(define-remapped-keys
    '(("(Firefox|Chrome|qutebrowser)"
       ("C-n"   . "Down")
       ("C-p"   . "Up")
       ("C-f"   . "Right")
       ("C-b"   . "Left")
       ("C-v"   . "Next")
       ("M-v"   . "Prior")
       ("M-w"   . "C-c")
       ("C-w"   . "C-x")
       ("C-y"   . "C-v")
       ("M-<"   . "Home")
       ("M->"   . "End")
       ("C-a"   . "Home")
       ("C-e"   . "End")
       ("C-M-b" . "M-Left")
       ("C-M-f" . "M-Right")
       ("C-g" . "ESC")
       ("C-k"   . ("C-S-End" "C-x")))))


(run-shell-command "~/.fehbg")

(require :pamixer)
(require :pass)

(require :screenshot)
(pamixer:volume-down 10)

(defun get-brightness ()
  (let ((c (parse-integer (string-trim
			   (string #\newline)
			   (run-shell-command "brightnessctl g" t))))
	(m (parse-integer (string-trim
			   (string #\newline)
			   (run-shell-command "brightnessctl m" t)))))
    (format nil "~3,1f%" (* (/ c m) 100))))

(defun set-brightness (x)
  (run-shell-command (concat "brightnessctl s " x))
  (message (concat "Brightness: " (get-brightness))))

(defcommand my-volume-down () ()
  (pamixer:volume-down 5)
  (message (format nil "Volume: ~s%" (pamixer:get-volume))))

(defcommand my-volume-up () ()
  (pamixer:volume-up 5)
  (message (format nil "Volume: ~s%" (pamixer:get-volume))))

(defcommand my-toggle-mute () ()
  (pamixer:toggle-mute)
  (if (pamixer:get-mute)
      (message "Mute: ^2ON")
      (message "Mute: ^1OFF")))

(defcommand my-brightness-up () ()
  (set-brightness "+5%"))

(defcommand my-brightness-down () ()
  (set-brightness "5%-"))

(defcommand my-brightness-one () ()
  (set-brightness "1"))


(define-key *top-map* (kbd "XF86AudioMute") "my-toggle-mute")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "my-volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "my-volume-down")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "my-brightness-up")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "my-brightness-down")

(defun my-screenshoot-command-wrapper (screenshot-command ss-name)
  (apply screenshot-command (list (concat
				   "~/Pictures/Screenshots/"
				   (string-trim
				    (string #\newline)
				    (run-shell-command "date +%Y-%M-%d-%T" t))
				   "-" (or ss-name "untitled")
				   ".png"))))


(defcommand my-screenshot-area (ss-name)
    ((:string "Screenshot name: "))
  (my-screenshoot-command-wrapper #'screenshot:screenshot-area ss-name))

(defcommand my-screenshot-window (ss-name)
    ((:string "Screenshot name: "))
  (my-screenshoot-command-wrapper #'screenshot:screenshot-window ss-name))

(defcommand my-screenshot (ss-name)
    ((:string "Screenshot name: "))
  (my-screenshoot-command-wrapper #'screenshot:screenshot ss-name))

(defvar *misc-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "0") "my-toggle-mute")
    (define-key m (kbd "+") "my-volume-up")
    (define-key m (kbd "=") "my-volume-up")
    (define-key m (kbd "-") "my-volume-down")

    (define-key m (kbd "p") "pass-copy-menu")
    (define-key m (kbd "C-p") "pass-copy-menu")
    (define-key m (kbd "g") "pass-generate")
    (define-key m (kbd "C-g") "pass-generate")

    (define-key m (kbd "C-a") "my-screenshot-area")
    (define-key m (kbd "a") "my-screenshot-area")
    (define-key m (kbd "C-w") "my-screenshot-window")
    (define-key m (kbd "w") "my-screenshot-window")
    (define-key m (kbd "C-s") "my-screenshot")
    (define-key m (kbd "s") "my-screenshot")
    
    m))




(define-key *root-map* (kbd "C-m") '*misc-keymap*)

;; 
;; ;;; MODE-LINE
(require :cpu)
(require :mem)
(require :net)

(require :stumptray)
(stumptray:add-mode-line-hooks)

(defun update-stumptray-position (&rest args)
  (setf (symbol-value (find-symbol "*TRAY-HEAD-SELECTION-FN*" :stumptray))
	(if (>= (list-length (stumpwm:screen-heads (stumpwm:current-screen))) 2)
	    #'second
	    #'first)))

(stumpwm:add-hook stumpwm:*new-head-hook* 'update-stumptray-position)
(update-stumptray-position)

(defun get-battery-status ()
  (let* ((state (string-trim
		 (string #\newline)
		 (run-shell-command			     
		  (concat "upower -i /org/freedesktop/UPower/devices/battery_BAT0 "
			  "| grep state: "
			  "| awk '{print $2}'")
		  t)))
	 (perc (string-trim
		(string #\newline)
		(run-shell-command (concat
				    "upower -i /org/freedesktop/UPower/devices/battery_BAT0 "
				    "| grep perc "
				    "| awk '{print $2}'")
				   t)))
	 (perc-num (parse-integer (aref (nth-value 1 (cl-ppcre:scan-to-strings "^(.*)\%$" perc)) 0))))
    (format
     nil
     (concat
      "BAT:  "
      (cond
	((and (equal state "discharging")
	      (<= perc-num 20))
	 "^1[-~a]^]")
	((equal state "discharging") "[-~a]")
	((equal state "charging") "[+~a]")
	((equal state "fully-charged") "[~a]")
	(t (concat state " [~a]"))))
     perc))) 



(setq *screen-mode-line-format* (list "[^B%n^b] %W  ^> %C | %M | %l | "
				      '(:eval (get-battery-status))
				      "      " ;; empty space for stumptray icons
				      ))

(require :yason)   ;; json parser
(require :drakma)  ;; http client


(defcommand kto-hakuje-p () ()
  (let* ((response (yason:parse
		    (flexi-streams:octets-to-string
		     (drakma:http-request "https://whois.at.hsp.sh/api/now"))))
	 (users (gethash "users" response))
	 (unknown-devices (gethash "unknown_devices" response)))
    (message (cond
	       ((and (= (list-length users) 0)
		     (= unknown-devices 0))
		"Spejs jest pusty!")
	       ((and (= (list-length users) 0)
		     (> unknown-devices 0))
		(format nil "W spejsie jest nieznanych ~a urzadzen." unknown-devices))
	       ((and (> (list-length users) 0)
		     (= unknown-devices 0))
		(format nil "W spejsie jest ~a." users))
	       ((and (> (list-length users) 0)
		     (> unknown-devices 0))
		(format nil "W spejsie jest ~a oraz ~a nieznane urzadzenia." users unknown-devices))))))


(define-key *misc-keymap* (kbd "h") "kto-hakuje-p")
(define-key *misc-keymap* (kbd "C-h") "kto-hakuje-p")


(defun emacs-server-p ()
  (let ((status-code (caddr
		      (multiple-value-list
		       (uiop:run-program "ls /run/user/$(id -u)/emacs/server"
					 :ignore-error-status T)))))
    (= status-code 0)))

(stumpwm:defcommand emacs-start-server (&optional (show-message T) (wait-for-start NIL)) ()
 (let ((mess (if (not (emacs-server-p))
		 (progn (stumpwm:run-shell-command "emacs --daemon" wait-for-start)
			"Emacs server is starting....")
		 "Emacs server is running already!")))
   (when show-message (message mess))))

(stumpwm:defcommand emacs-stop-server (&optional (show-message T)) ()
  (let ((mess (if (emacs-server-p)
		  (progn (stumpwm:run-shell-command "emacsclient -e \"(server-force-delete)\"")
			 "Emacs server gone away :(....")
		  "Emacs server wasn't alive!")))
    (when show-message (message mess))))


(stumpwm:defcommand emacs-restart-server (&optional (show-message T)) ()
  (emacs-stop-server NIL)
  (emacs-start-server show-message))

(defun postwalk (fun tree)
  (if (consp tree)
      (loop :for a :in tree
	    :if (consp a)
	      :collect (postwalk fun a)
	    :else
	      :collect (funcall fun a))
      (funcall fun tree)))

(defmacro eval-emacs-sexp (sexp
			   &key (create-new-frame NIL))
  `(stumpwm:run-shell-command
    (format nil "emacsclient~{ ~A~} '~A'"
	    (list ,(if create-new-frame "-c" "")
		  "-e")
	    (postwalk (lambda (x)
			(cond
			 ((stringp x) (concat "\"" x "\""))
			 ((symbolp x) (string-downcase (string x)))
			 (T x)))
		      ,sexp))
    ,(not create-new-frame)))


(defmacro defcommand-from-emacs (name
				 (&rest args)
				 (&rest interactive-args)
				 (&key (create-new-frame T) (output-wrapper NIL))
				 &body body)
  `(stumpwm:defcommand ,name ,args ,interactive-args
     (when (not (emacs-server-p))
       (emacs-start-server NIL T))

     ,(let ((x `(eval-emacs-sexp (progn ,@body)
				 :create-new-frame ,create-new-frame)))
	(cond
	  ((and create-new-frame output-wrapper)
	   (error "Cannot wrap the output, becaouse create-new-frame is T."))
	  (output-wrapper `(funcall ,output-wrapper ,x))
	  (T `(funcall (lambda (x) (progn x nil)) ,x))))))


(defcommand-from-emacs emacs-client () () ()
  nil)

(defcommand-from-emacs emacs-calc () () ()
  '(full-calc))

(defcommand-from-emacs emacs-org-agenda () () ()
  '(org-agenda-list))

(defcommand-from-emacs emacs-mu4e () () ()
  '(mu4e))

(defcommand-from-emacs emacs-shell () () ()
  '(shell))

(defcommand-from-emacs emacs-eshell () () ()
  '(eshell))

(defvar *emacs-keymap*
  (let ((e (make-sparse-keymap)))
    (define-key e (kbd "a") "emacs-org-agenda")
    (define-key e (kbd "C-a") "emacs-org-agenda")
    (define-key e (kbd "c") "emacs-calc")
    (define-key e (kbd "C-c") "emacs-calc")
    (define-key e (kbd "m") "emacs-mu4e")
    (define-key e (kbd "C-m") "emacs-mu4e")

    e))




(define-key *root-map* (kbd "C-e") '*emacs-keymap*)
(define-key *root-map* (kbd "e") "emacs-client")
(define-key *root-map* (kbd "c") "emacs-shell")
(define-key *root-map* (kbd "C") "emacs-eshell")


(emacs-start-server nil)
