(define-module (jd desktops base)
  #:use-module (jd home services polkit)
  #:use-module (jd home services desktop)  
  #:use-module (jd services polkit)
  
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-package-modules wm gnome gnupg networking virtualization
		     lisp lisp-xyz cups)

(use-service-modules cups desktop networking ssh xorg
		     docker virtualization pm sound dbus
		     nix)

(define-public %jd-base-home-services
  (list
   (service home-xdg-mime-applications-service-type
	    (home-xdg-mime-applications-configuration
	     (default '((inode/directory . emacs-desktop.desktop)
			(application/pdf . emacs-desktop.desktop)))
	     (desktop-entries
	      (list (xdg-desktop-entry
		     (file "emacs-desktop")
		     (name "Emacs")
		     (type 'application)
		     (config
		      '((exec . "emacsclient -a emacs %u"))))))))

   (service home-redshift-service-type
            (home-redshift-configuration
             (location-provider 'manual)
             (latitude 51.919438)
             (longitude 19.145136))) ;; Poland

   (simple-service 'some-useful-env-vars-service
          	   home-environment-variables-service-type
          	   `(("GTK_THEME" . "Adwaita:dark")
		     ("VISUAL" . "emacsclient")
		     ("EDITOR" . "emacsclient")
		     ("PATH" . "$HOME/.bin:$HOME/.npm-global/bin:$PATH")
		     ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")
		     ("SBCL_HOME" . "/run/current-system/profile/lib/sbcl/")))

   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
	     (pinentry-program
              (file-append pinentry "/bin/pinentry"))
             (ssh-support? #t)
             (default-cache-ttl 28800)
             (max-cache-ttl 28800)
             (default-cache-ttl-ssh 28800)
             (max-cache-ttl-ssh 28800)))

   (service home-dbus-service-type)

   (service home-desktop-service-type)
   (service home-polkit-gnome-service-type)))


(define-public %jakub-user
  (user-account
   (name "jakub")
   (comment "Jakub Dlugosz")
   (group "users")
   (home-directory "/home/jakub")
   (supplementary-groups '("wheel"    ;; sudo
                           "netdev"   ;; network devices
                           "kvm"
                           "libvirt"
			   "tty"
			   "input"
                           "docker"
                           "audio"    ;; control audio devices
                           "video"    ;; access to webcam
			   "dialout"  ;; access to /dev/ttyUSBX devices
			   "adbusers"
			   ))))

(define-public %jd-base-user-accounts
  (cons*
   %jakub-user
   %base-user-accounts))

(define-public %stumpwm-packages
  (list sbcl
	sbcl-dbus
	stumpwm+slynk

	sbcl-stumpwm-screenshot
	sbcl-stumpwm-pamixer
	sbcl-stumpwm-pass

	sbcl-stumpwm-cpu
	sbcl-stumpwm-mem
	sbcl-stumpwm-net
	sbcl-stumpwm-battery-portable
        sbcl-stumpwm-stumptray

	sbcl-drakma
	sbcl-yason

	(list stumpwm "lib")))

(define-public %root-packages
  (specifications->packages '("emacs"
			      "emacs-exwm"
			      "stow"
			      "bluez"
			      "bluez-alsa"
			      "exfat-utils"
			      "git"
			      "xf86-input-libinput"
			      "intel-vaapi-driver"
			      "libva-utils" ;; vainfo
			      "nss-certs"
			      "nix")))

(define-public %jd-base-packages
  (append %root-packages
	  %stumpwm-packages
	  %base-packages))

(define-public %jd-base-services
  (cons*
   (service openssh-service-type)

   (set-xorg-configuration
    (xorg-configuration			;for Xorg
     (keyboard-layout (keyboard-layout "pl"))))

   (service network-manager-service-type
	    (network-manager-configuration
	     (vpn-plugins (list network-manager-openvpn))))
   
   (simple-service 'dbus-packages dbus-root-service-type (list blueman
							       virt-manager))
   (service bluetooth-service-type
	    (bluetooth-configuration
	     (auto-enable? #t)))

   (service docker-service-type)
   (service libvirt-service-type
	    (libvirt-configuration
	     (unix-sock-group "libvirt")))
   (service virtlog-service-type)

   (service cups-service-type
	    (cups-configuration
	     (web-interface? #t)
	     (extensions
              (list cups-filters))))

   (service thermald-service-type)
   (service tlp-service-type
	    (tlp-configuration
	     (cpu-boost-on-ac? #t)
	     (wifi-pwr-on-bat? #t)))

   (service nix-service-type)
   
   polkit-network-manager-service
   
   ;; %desktop-services
   (modify-services %desktop-services
		    (guix-service-type config => (guix-configuration
						  (inherit config)
						  (substitute-urls
						   (append (list "https://substitutes.nonguix.org")
							   %default-substitute-urls))
						  (authorized-keys
						   (append (list (plain-file "non-guix.pub"
									     "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
							   %default-authorized-guix-keys))))
		    (delete network-manager-service-type))))

;; Odin is a base for my operating systems
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
