(define-module (jd home services desktop)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services dotfiles)
  #:use-module (guix gexp)

  #:use-module (jd packages fonts))

(define (desktop-profile-service config)
  (append
   (list font-terminus-ttf)
   (specifications->packages '("sway"
			       ;; "swayfx"
			       "swaylock"
			       "swayidle"
			       "swaynotificationcenter"
			       "waybar"
			       "fuzzel"
			       "foot"
			       "mako"
			       "wl-clipboard"
			       "wlsunset"
			       "grimshot"
			       "swappy"

			       "htop"
			       "distrobox"
			      
			       "udiskie"
			       ;; rest
			       "qutebrowser"
			       "ungoogled-chromium"
			       "firefox"
			       "tor-client"
			       "signal-desktop"

			       "pulsemixer"
			       "pavucontrol"
			       "alsa-utils"

			       "virt-manager"

			       "flatpak"
			       "redshift"
			       "fontmanager"

			       "polybar"

			       "blueman"

			       "nomacs"

			       "xdg-utils"
			       "xdg-dbus-proxy"
			       "xdg-desktop-portal-gtk"
			       "xdg-desktop-portal-wlr"
			       "glib:bin"
			       "gtk+:bin"
			       "gnome-keyring"
			       "shared-mime-info"
			       "libnotify"
			       "dconf"
			       "hicolor-icon-theme"

			       "dunst"

			       ; "gimp"
			       ; "inkscape"

			       "mpv"
			       "youtube-dl"

			       "pamixer"
			       "playerctl"
			       "scrot"
			       "brightnessctl"
			       "upower"
			       "tlp"
			       "feh"
			       "alacritty"

			       "curl"
			       "wget"
			       "zip"
			       "unzip"
			       "qrencode"
			       "trash-cli"
			       "pandoc"
			       "password-store"
			       "oath-toolkit"
			       ;; "pinentry"

			       "syncthing"
			       "syncthing-gtk"

			       "xmodmap"
			       "xrandr"
			       "arandr"
			       "xss-lock"
			       "libinput"
			       "xinput"
			       "xprop"
			       "rlwrap"

			       "nheko"
			       "quassel"
			       "inkscape"
			       "gimp"
			       "libreoffice"
			       "steam"))))

(define (desktop-environment-variables-service config)
  `(("GTK_THEME" . "Adwaita:dark")
    ("VISUAL" . "emacsclient")
    ("EDITOR" . "emacsclient")
    ("PATH" . "$HOME/.bin:$HOME/.local/bin:$HOME/.npm-global/bin:$PATH")
    ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")
    ("SBCL_HOME" . "/run/current-system/profile/lib/sbcl/")

    ;; Set Wayland-specific environment variables (taken from RDE)
    ("XDG_CURRENT_DESKTOP" . "sway")
    ("XDG_SESSION_TYPE" . "wayland")
    ("RTC_USE_PIPEWIRE" . "true")
    ("SDL_VIDEODRIVER" . "wayland")
    ("MOZ_ENABLE_WAYLAND" . "1")
    ("CLUTTER_BACKEND" . "wayland")
    ("ELM_ENGINE" . "wayland_egl")
    ("ECORE_EVAS_ENGINE" . "wayland-egl")
    ("QT_QPA_PLATFORM" . "wayland-egl"))) ;; QT_QPA_PLATFORM=xcb

(define (desktop-xdg-mime-applications-service config)
  (home-xdg-mime-applications-configuration
   (default '((inode/directory . emacsclient.desktop)
	      (application/pdf . emacsclient.desktop)
	      (x-scheme-handler/http= . firefox.desktop)
	      (x-scheme-handler/https= . firefox.desktop)))
   ;; (desktop-entries
   ;;  (list (xdg-desktop-entry
   ;; 	     (file "emacs-desktop")
   ;; 	     (name "Emacs")
   ;; 	     (type 'application)
   ;; 	     (config
   ;; 	      '((exec . "emacsclient -a emacs %u"))))))
   ))

(define (desktop-gpg-agent-service config)
  (home-gpg-agent-configuration
   (pinentry-program
    (file-append pinentry-gnome3 "/bin/pinentry-gnome3"))
   (ssh-support? #t)
   (default-cache-ttl 28800)
   (max-cache-ttl 28800)
   (default-cache-ttl-ssh 28800)
   (max-cache-ttl-ssh 28800)))

(define (desktop-gpg-agent-service config)
  (home-gpg-agent-configuration
   (pinentry-program
    (file-append pinentry-gnome3 "/bin/pinentry-gnome3"))
   (ssh-support? #t)
   (default-cache-ttl 28800)
   (max-cache-ttl 28800)
   (default-cache-ttl-ssh 28800)
   (max-cache-ttl-ssh 28800)))

(define (desktop-dotfiles-service config)
  (home-dotfiles-configuration
   (directories '("./files"))))

(define-public home-desktop-service-type
  (service-type (name 'home-desktop)
                (extensions (list (service-extension home-profile-service-type
						     desktop-profile-service)
				  (service-extension home-environment-variables-service-type
          					     desktop-environment-variables-service)
				  (service-extension home-pipewire-service-type
						     (lambda (_) (home-pipewire-configuration)))
				  (service-extension home-xdg-mime-applications-service-type
					 	     desktop-xdg-mime-applications-service)
				  (service-extension home-gpg-agent-service-type
						     desktop-gpg-agent-service)
				  (service-extension home-dotfiles-service-type
						     desktop-dotfiles-service)
				  ))
                (default-value #f)
                (description "Runs desktop services.")))

