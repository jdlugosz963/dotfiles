(define-module (jd desktops berserker)
  #:use-module (jd desktops base)
  #:use-module (jd packages linux)
  #:use-module (gnu)
  #:use-module (gnu home))

(define home
  (home-environment
   (services %jd-base-home-services)))

(define system
  (operating-system
   (inherit odin-non-free)
   (kernel-loadable-modules (list xmm7360-pci))
   (host-name "berserker")

   
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))))
  (mapped-devices (list (mapped-device
                          (source (uuid
                                   "1f2b1bf2-89fe-4e2c-8b40-c460572bb776"))
                          (target "crypthome")
                          (type luks-device-mapping))))

  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "66396ba1-bda9-46bd-ab26-5edc46b437d6"
                                  'ext4))
                         (type "ext4"))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "482A-B117"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/home")
                         (device "/dev/mapper/crypthome")
                         (type "ext4")
                         (dependencies mapped-devices)) %base-file-systems))))


(if (getenv "JD_HOME") home system)
