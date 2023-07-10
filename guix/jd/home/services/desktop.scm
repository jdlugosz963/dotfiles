(define-module (jd home services desktop)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages gnome)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services desktop)
  #:use-module (guix gexp))

(define (desktop-profile-service config)
  (specifications->packages '("udiskie"
			      "syncthing-gtk"
			      "network-manager-applet")))

(define (desktop-shepherd-services config)
  (list (shepherd-service
         (documentation "Udiskie daemon with tray.")
         (provision '(udiskie))
         (start #~(make-forkexec-constructor
                   (list #$(file-append udiskie
					"/bin/udiskie")
			 "-t")))
         (stop #~(make-kill-destructor)))
	
	(shepherd-service
         (documentation "Syncthing daemon with tray.")
         (provision '(syncthing-gtk))
         (start #~(make-forkexec-constructor
                   (list #$(file-append syncthing-gtk
					"/bin/syncthing-gtk")
			 "-m")))
         (stop #~(make-kill-destructor)))

	(shepherd-service
         (documentation "NetworkManager tray.")
         (provision '(nm-applet))
         (start #~(make-forkexec-constructor
                   (list #$(file-append network-manager-applet
					"/bin/nm-applet"))))
         (stop #~(make-kill-destructor)))))

(define-public home-desktop-service-type
  (service-type (name 'home-udiskie)
                (extensions (list (service-extension home-profile-service-type
						     desktop-profile-service)
				  (service-extension home-shepherd-service-type
						     desktop-shepherd-services)))
                (default-value #f)
                (description "Runs desktop services.")))

