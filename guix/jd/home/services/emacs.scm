(define-module (jd home services emacs)
  #:use-module (jd packages emacs)

  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)

  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services desktop)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles))

(define-configuration home-emacs-service-configuration
  (emacs-package
   (package emacs-next-pgtk)
   "Emacs packages"))

(define (emacs-profile-service config)
  (append (list emacs-org-roam-ui
		emacs-lsp-mode!
		(home-emacs-service-configuration-emacs-package
		 config))
	  (specifications->packages '("emacs-sway"
				      "emacs-shackle"
				      "emacs-ytdl"
				      "emacs-desktop-environment"
				      "emacs-exwm"
				      "emacs-perspective"
				      "emacs-mu4e-alert"
				      "mu"
				      "isync"
				      "emacs-htmlize"
				      "emacs-bluetooth"
				      "emacs-nov-el"
				      "emacs-password-store"
				      "emacs-mastodon"
				      "emacs-elfeed"
				      "emacs-pdf-tools"
				      "emacs-emms"
				      "emacs-all-the-icons-dired"
				      "emacs-vterm"
				      "emacs-restclient"
				      "emacs-magit"
				      "emacs-neotree"
				      "emacs-projectile"
				      "emacs-company-box"
				      "emacs-company"
				      "emacs-docker"
				      "emacs-yaml-mode"
				      "emacs-web-mode"
				      "emacs-flycheck"
				      "emacs-tide"
				      "emacs-cider"
				      "emacs-typescript-mode"
				      "emacs-pyvenv"
				      "emacs-geiser-guile"
				      "emacs-racket-mode"
				      "emacs-geiser-racket"
				      "emacs-geiser"
				      "emacs-sly"
				      "emacs-rainbow-delimiters"
				      "emacs-paredit"
				      ;; "emacs-lsp-ivy"
				      ;; "emacs-lsp-mode"
				      "emacs-org-caldav"
				      "emacs-org-pomodoro"
				      "emacs-org-roam"
				      "emacs-org-roam-bibtex"
				      "emacs-org-superstar"
				      "emacs-org"
				      "emacs-ox-pandoc"
				      "emacs-beacon"
				      "emacs-all-the-icons"
				      "emacs-which-key"
				      "emacs-counsel"
				      "emacs-ivy"
				      "emacs-hl-todo"
				      "emacs-diminish"
				      "emacs-solarized-theme"
				      "font-terminus"
				      "emacs-undo-tree"
				      "emacs-hydra"
				      "emacs-multiple-cursors"
				      "emacs-general"
				      "emacs-guix"     
				      "emacs-doom-modeline"
				      "emacs-use-package"))))

(define (emacs-shepherd-services config)
  (list (shepherd-service
         (documentation "Emacs daemon.")
         (provision '(emacs))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (home-emacs-service-configuration-emacs-package
					 config)
					"/bin/emacs")
			 "--fg-daemon")))
         (stop #~(make-kill-destructor)))))

(define-public home-emacs-service-type
  (service-type (name 'home-emacs)
                (extensions (list (service-extension home-profile-service-type
						     emacs-profile-service)
				  (service-extension home-shepherd-service-type
						     emacs-shepherd-services)
				  ))
                (default-value (home-emacs-service-configuration))
                (description "Runs emacs daemon service.")))

