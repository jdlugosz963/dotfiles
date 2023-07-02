(define-module (jd desktops base)
  #:use-module (gnu)
  #:use-module (jd packages vpn)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-package-modules wm gnome)
(use-service-modules cups desktop networking ssh xorg
		     docker virtualization pm sound)

;; Odin is a base operating system
(define-public odin
  (operating-system
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (list linux-firmware))

   (locale "en_US.utf8")
   (timezone "Europe/Warsaw")
   (keyboard-layout (keyboard-layout "pl"))
   (host-name "odin")
   
   (users (cons* (user-account
		  (name "jakub")
		  (comment "Jakub Dlugosz")
		  (group "users")
		  (home-directory "/home/jakub")
		  (supplementary-groups '("wheel" "netdev" "audio" "video")))
		 %base-user-accounts))

   (packages (append (specifications->packages '("emacs"
						 "emacs-exwm"
						 
						 "brightnessctl"

						 "bluez"
						 "bluez-alsa"

						 "xf86-input-libinput" 
						 
						 "nss-certs"))

		     %base-packages))
   
   (services
    (append (list
	     (service xfce-desktop-service-type)
	     (service openssh-service-type)
	     (set-xorg-configuration
	      (xorg-configuration (keyboard-layout keyboard-layout)))

	     (service network-manager-service-type
		      (network-manager-configuration
		       (vpn-plugins (list
				     network-manager-pptp))))
	     
	     (service bluetooth-service-type
		      (bluetooth-configuration
		       (auto-enable? #t)))

	     (service docker-service-type)
	     (service libvirt-service-type
		      (libvirt-configuration
		       (unix-sock-group "libvirt")
		       (tls-port "16555")))

	     (service sane-service-type)
	     (service cups-service-type
		      (cups-configuration
		       (web-interface? #t)))

	     (service thermald-service-type)
	     (service tlp-service-type
		      (tlp-configuration
		       (cpu-boost-on-ac? #t)
		       (wifi-pwr-on-bat? #t))))

	    (modify-services %desktop-services
                             (delete network-manager-service-type))))
   (bootloader (bootloader-configuration
		(bootloader grub-bootloader)
		(targets (list "/boot/efi"))
		(keyboard-layout keyboard-layout)))
   
   (file-systems (cons* (file-system
			 (mount-point "/tmp")
			 (device "none")
			 (type "tmpfs")
			 (check? #f))
			%base-file-systems))))
