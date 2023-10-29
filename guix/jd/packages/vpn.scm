(define-module (jd packages vpn)
  #:use-module (jd utils)
  
  #:use-module (gnu packages)
  #:use-module (gnu packages samba) ;; PPP
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gtk)
  
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix inferior)
  #:use-module (guix channels))


(define channels
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (commit
          "9b77bd0b9b4f3de69390da0ba7db5b9dbc01e554"))))

(define inferior
  (inferior-for-channels channels))

(define ppp-2.4.9 (car (lookup-inferior-packages inferior "ppp")))


(define-public pptp-client
  (package
    (name "pptp-client")
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/pptpclient/git")
             (commit version)))
       (sha256
        (base32 "0qixs1dxrr1x4sgi22250231p9kqi8l8ifawxn6ws1d3p7mc6ggh"))))
    
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
	 (delete 'check)
	 (add-before 'build 'change-location-of-pppd
           (lambda* (#:key outputs #:allow-other-keys)
	     (substitute* "Makefile"
               (("^PPPD.=.*$")
                (string-append "PPPD = "
			       (which "pppd")))
	       (("^IP.=.*$")
                (string-append "IP = "
			       (which "ip"))))))
	 (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin")))
               (mkdir-p sbin)
               (install-file "pptp" sbin)
               #t))))))
    (inputs
     (list perl))
    (native-inputs
     (list ppp-2.4.9
	   iproute))
    (home-page "https://sourceforge.net/projects/pptpclient/")
    (synopsis "PPTP-Client")
    (description
     "pptp is an implementation of the PPTP protocol for Linux and
other Unix systems.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-pptp")))))


;; ================================

(define-public network-manager-pptp
  (package
    (name "network-manager-pptp")
    (version "1.2.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/NetworkManager-pptp.git")
             (commit version)))
       (file-name (git-file-name name version))
       
       (sha256
        (base32 "1prl14106kfl5x56v9adwi4wqwrh6k3chkimjpd0clwy3drywhcr"))
       (patches (jd-search-patches "nm-pptp-autogen.patch"))))
    
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-absolute-paths"
					;        "--localstatedir=/var"
			   "--with-gtk4=yes")
       #:phases
       (modify-phases %standard-phases
	 (add-after 'configure 'patch-path
	   (lambda* (#:key inputs #:allow-other-keys #:rest args)
	     (let* ((pppd (search-input-file inputs "/sbin/pppd"))
		    (pptp (search-input-file inputs "/sbin/pptp"))
		    (modprobe (search-input-file inputs "/bin/modprobe"))
		    (pretty-pppd (string-append "\"" pppd "\""))
		    (pretty-pptp (string-append "\"" pptp "\"")))

	       (substitute* "src/nm-pptp-service.c"
		 (("\"/usr/local/sbin/pppd\"") pretty-pppd)
		 (("\"/usr/sbin/pppd\"") pretty-pppd)
		 (("\"/sbin/pppd\"") pretty-pppd)
		 (("\"/usr/local/sbin/pptp\"") pretty-pptp)
		 (("\"/usr/sbin/pptp\"") pretty-pptp)
		 (("\"/sbin/pptp\"") pretty-pptp)
		 (("/sbin/modprobe") modprobe))))))))
    (native-inputs
     (list intltool
           `(,glib "bin")
	   glib
	   `(,gtk "bin")
           pkg-config))
    (inputs
     (list autoconf
	   autoconf-archive
	   gnulib
	   gnu-gettext
	   automake 
	   gtk+
           gtk
           kmod
	   libtool
	   network-manager
	   libnma
	   libsecret
	   ppp-2.4.9
           pptp-client))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "PPTP plug-in for NetworkManager")
    (description
     "This extension of NetworkManager allows it to take care of connections
to virtual private networks (VPNs) via pptp.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-pptp")))))
