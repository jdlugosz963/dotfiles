(define-module (jd desktops berserker)
  #:use-module (jd desktops base)
  #:use-module (gnu)
  #:use-module (gnu home))

(define home
  (home-environment
   (services %jd-base-home-services)))

(define system
  (operating-system
   (inherit odin-non-free)
   (host-name "berserker")

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))))

   (swap-devices (list (swap-space
                        (target (uuid
                                 "70ee2477-66ff-4856-aebf-8a77053c4462")))))

   (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "BE9B-69F0"
                                       'fat32))
                         (type "vfat"))

			(file-system
                         (mount-point "/")
                         (device (uuid
                                  "d8783299-c180-4ca3-9c56-9826797f928d"
                                  'ext4))
                         (type "ext4"))

			%base-file-systems))))


(if (getenv "JD_HOME") home system)
