(define-module (jd desktops mimir)
  #:use-module (jd desktops base)
  #:use-module (gnu)
  #:use-module (gnu home))

(define-public home
  (home-environment
   (services %jd-base-home-services)))

(define-public system
  (operating-system
   (inherit odin-non-free)
   (host-name "mimir")))

