(define-module (jd home services polkit)
  #:use-module (gnu packages)
  #:use-module (gnu packages polkit)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp))


(define (polkit-gnome-profile-service config)
  (specifications->packages '("polkit-gnome")))

(define (polkit-gnome-shepherd-service config)
    (list (shepherd-service
         (documentation "Polkit gnome service.")
         (provision '(polkit-gnome))
         (start #~(make-forkexec-constructor
                   (list #$(file-append polkit-gnome
					"/libexec/polkit-gnome-authentication-agent-1"))))
         (stop #~(make-kill-destructor)))))

(define-public home-polkit-gnome-service-type
  (service-type (name 'home-polkit-gnome)
                (extensions (list (service-extension home-profile-service-type
						     polkit-gnome-profile-service)
				  (service-extension home-shepherd-service-type
						     polkit-gnome-shepherd-service)))
                (default-value #f)
                (description "Runs the PolicyKit-gnome service.")))

