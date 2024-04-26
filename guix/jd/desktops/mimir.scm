(define-module (jd desktops mimir)
  #:use-module (jd desktops base)
  #:use-module (gnu)
  #:use-module (gnu home))

(define home
  (home-environment
   (services %jd-base-home-services)))

(define system
  (operating-system
   (inherit odin-non-free)
   (host-name "mimir")

   (bootloader (bootloader-configuration
		(bootloader grub-efi-bootloader)
		(targets (list "/boot/efi"))))

   (swap-devices (list (swap-space
			(target (uuid
				 "5402ec34-85b8-4716-9c37-3d38c452ef98")))))

   (file-systems (cons* (file-system
			 (mount-point "/boot/efi")
			 (device (uuid "1A88-DB36"
                                       'fat32))
			 (type "vfat"))
			(file-system
			 (mount-point "/")
			 (device (uuid
                                  "640c44ea-125f-4410-a8de-3ec0fb3656c4"
                                  'ext4))
			 (type "ext4")) %base-file-systems))))

(if (getenv "JD_HOME") home system)

