(define-module (jd home services udiskie)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp))

(define (udiskie-profile-service config)
  (specifications->packages '("udiskie")))

(define (udiskie-shepherd-service config)
  (list (shepherd-service
         (documentation "udiskie daemon")
         (provision '(udiskie))
         (start #~(make-forkexec-constructor
                   (list #$(file-append udiskie
					"/bin/udiskie")
			 "-t")))
         (stop #~(make-kill-destructor)))))

(define-public home-udiskie-service-type
  (service-type (name 'home-udiskie)
                (extensions (list (service-extension home-profile-service-type
						     udiskie-profile-service)
				  (service-extension home-shepherd-service-type
						     udiskie-shepherd-service)))
                (default-value #f)
                (description "Runs udiskie daemon.")))

