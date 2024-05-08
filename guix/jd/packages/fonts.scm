(define-module (jd packages fonts)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:))

(define-public font-terminus-ttf
  (package
    (name "font-terminus-ttf")
    (version "4.49.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.ax86.net/terminus-ttf/files/"
				  version "/terminus-ttf-" version ".zip"))
              (sha256
               (base32
                "0xvm4mcf0gx7df35swqwxb7a5h3z0gf6dkg6zy84b6nrk0fr5b8f"))))
    (build-system font-build-system)
    (home-page "https://files.ax86.net/terminus-ttf/")
    (synopsis "Terminus TTF Font")
    (description
     "Terminus TTF is a TrueType version of Terminus Font, a fixed-width bitmap font optimized for long work with computers.")
    (license license:gpl2+)))

