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
   
   (mapped-devices (list (mapped-device
                          (source (uuid
                                   "ef754148-6f23-4d7e-92b4-7abbfe0ae201"))
                          (target "cryptroot")
                          (type luks-device-mapping))))

   (file-systems (cons* (file-system
			 (mount-point "/boot/efi")
			 (device (uuid "695E-FE22"
                                       'fat32))
			 (type "vfat"))
			(file-system
			 (mount-point "/")
			 (device "/dev/mapper/cryptroot")
			 (type "ext4")
			 (dependencies mapped-devices)) %base-file-systems))))


(if (getenv "JD_HOME") home system)
