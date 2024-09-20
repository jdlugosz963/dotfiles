(define-module (jd desktops berserker)
  #:use-module (jd desktops base)
  #:use-module (jd packages linux)
  #:use-module (jd utils)
  #:use-module (gnu)
  #:use-module (gnu home))

(define-public home
  (home-environment
   (services %jd-base-home-services)))

(define-public system
  (operating-system
   (inherit odin-non-free)
   (kernel-loadable-modules (list xmm7360-pci))
   (host-name "berserker")))

