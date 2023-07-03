(define-module (jd desktops base)
  #:use-module (gnu)
  #:use-module (jd packages vpn)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-package-modules wm gnome)
(use-service-modules cups desktop networking ssh xorg
		     docker virtualization pm sound)

(define-public %jd-base-user-accounts
  (cons* (user-account
	  (name "jakub")
	  (comment "Jakub Dlugosz")
	  (group "users")
	  (home-directory "/home/jakub")
	  (supplementary-groups '("wheel"
				  "netdev"
				  "audio"
				  "docker"
				  "kvm"
				  "libvirt"
				  "video")))
	 %base-user-accounts))

(define-public %jd-base-packages
  (append
   (specifications->packages '("emacs"
			       "emacs-exwm"
			       "stow"
			       "bluez"
			       "bluez-alsa"
			       "exfat-utils"
			       "git"
			       "xf86-input-libinput"
			       "intel-vaapi-driver"
			       "libva-utils"
			       "nss-certs"))
   %base-packages))

(define-public %jd-base-services
  (append (list
	   (service openssh-service-type)

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
		     (wifi-pwr-on-bat? #t)))

	   (service slim-service-type (slim-configuration
                                       (display ":0")
                                       (vt "vt7"))))

	  (modify-services %desktop-services
                           (delete network-manager-service-type)
			   (delete gdm-service-type))))

;; Odin is a base operating system
(define-public odin-free
  (operating-system
   (locale "en_US.utf8")
   (timezone "Europe/Warsaw")
   (keyboard-layout (keyboard-layout "pl"))
   (host-name "odin")
   
   (users %jd-base-user-accounts)
   (packages %jd-base-packages)
   (services %jd-base-services)

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

(define-public odin-non-free
  (operating-system
   (inherit odin-free)
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (list linux-firmware))))
