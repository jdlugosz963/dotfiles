(define-module (viking)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
 (inherit base-operating-system)
 (host-name "viking")

 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (target "/dev/sda")))

 (mapped-devices
  (list (mapped-device
         (source
          (uuid "75a1ebd7-13c2-442f-80ff-1b5e59522e29"))
         (target "cryptroot")
         (type luks-device-mapping))))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device "/dev/mapper/cryptroot")
          (type "ext4")
          (dependencies mapped-devices))
         %base-file-systems)))
