(define-module (jd desktops mimir)
  #:use-module (jd desktops base)
  #:use-module (gnu)
  #:use-module (gnu services desktop))


(operating-system
 (inherit odin-non-free)
 (host-name "mimir")

 (services (cons
	    (service xfce-desktop-service-type)
	    %jd-base-services))
 
 (bootloader (bootloader-configuration
	      (bootloader grub-efi-bootloader)
	      (targets (list "/boot/efi"))))

 
 (swap-devices (list (swap-space
		      (target (uuid
			       "658793cb-d374-426e-bcd5-00d032b003a0")))))

 (file-systems (cons* (file-system
		       (mount-point "/boot/efi")
		       (device (uuid "64D8-134F"
                                     'fat32))
		       (type "vfat"))
		      (file-system
		       (mount-point "/")
		       (device (uuid
                                "1f9304b0-5623-4248-ab66-534b5ac85876"
                                'ext4))
		       (type "ext4")) %base-file-systems)))

