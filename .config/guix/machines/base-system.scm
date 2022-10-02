(define-module (base-system)
  #:use-module (gnu)
  #:use-module (gnu system nss)
  #:use-module (gnu services pm)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management))
                                        ;    #:use-module (nongnu packages linux)
                                        ;    #:use-module (nongnu system linux-initrd))

(use-service-modules ssh)
(use-package-modules certs)
(use-service-modules desktop xorg)

(define-public base-operating-system
  (operating-system
   (host-name "base")
   (timezone "Europe/Warsaw")
   (locale "en_US.utf8")

   ;; Use non-free Linux and firmware
   ;;     (kernel linux)
   ;;     (firmware (list linux-firmware))
   ;;     (initrd microcode-initrd)

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets "/boot/efi")))
   ;; (keyboard-layout keyboard-layout)))

   ;; Guix doesn't like it when there isn't a file-systems
   ;; entry, so add one that is meant to be overridden
   (file-systems (cons*
                  (file-system
                   (mount-point "/tmp")
                   (device "none")
                   (type "tmpfs")
                   (check? #f))
                  %base-file-systems))
   
   ;; Users
   (users (cons (user-account
                 (name "jakub")
                 (comment "Jakub Dlugosz")
                 (group "users")
                 (home-directory "/home/jakub")
                 (supplementary-groups '(
                                         "wheel" 
                                         "netdev" 
                                         "kvm"
                                         "tty"
                                         "input"
                                         "docker"
                                         "lp"
                                         "audio"
                                         "video")))
                %base-user-accounts))
   
   
   ;; Packages
   (packages (append (list
                      git
                      exfat-utils
                      fuse-exfat
                      stow
                      vim
                      emacs
                      bluez
                      bluez-alsa
                      pulseaudio
                      tlp
                      xf86-input-libinput
                      nss-certs) ;; For https connection
                     %base-packages))
   
   
   ;; Services
   (services
    (append
     (list
      (service elogind-service-type)
      (service openssh-service-type)
      (service network-manager-service-type)
      (service slim-service-type)
      (service tlp-service-type)
      (bluetooth-service #:auto-enable? #t)
      (service docker-service-type)
      (service wpa-supplicant-service-type))
     %base-services))
   
   ;; Allow resolution of '.local' host names with mDNS
   (name-service-switch %mdns-host-lookup-nss)))
