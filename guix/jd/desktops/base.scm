(define-module (jd desktops base)
  #:use-module (jd packages vpn)
  #:use-module (jd home services polkit)
  #:use-module (jd home services udiskie)
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

(use-package-modules wm gnome gnupg)
(use-service-modules cups desktop networking ssh xorg
		     docker virtualization pm sound)


(define-public %jd-base-home-services
  (list
   (service home-xdg-mime-applications-service-type
	    (home-xdg-mime-applications-configuration
	     (default '((inode/directory . emacs-desktop.desktop)))
	     (default '((application/pdf . emacs-desktop.desktop)))
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
		      ("VISUAL" . "emacsclient -a emacs")
		      ("EDITOR" . "emacsclient -a emacs")
		      ("PATH" . "$HOME/.bin:$HOME/.npm-global/bin:$PATH")
		      ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")))

    (service home-gpg-agent-service-type
             (home-gpg-agent-configuration
	      (pinentry-program
                (file-append pinentry "/bin/pinentry"))
              (ssh-support? #t)
              (default-cache-ttl 28800)
              (max-cache-ttl 28800)
              (default-cache-ttl-ssh 28800)
              (max-cache-ttl-ssh 28800)))

    (service home-udiskie-service-type)
    (service home-polkit-gnome-service-type)))

(define-public %jd-base-user-accounts
  (cons*
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
			    )))
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
			       "libva-utils" ;; vainfo
			       "nss-certs"))
   %base-packages))

(define-public %jd-base-services
  (cons*
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
	     (unix-sock-group "libvirt")))

   (service cups-service-type
	    (cups-configuration
	     (web-interface? #t)))

   (service thermald-service-type)
   (service tlp-service-type
	    (tlp-configuration
	     (cpu-boost-on-ac? #t)
	     (wifi-pwr-on-bat? #t)))
   
   polkit-network-manager-service
   
   (modify-services %desktop-services
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
