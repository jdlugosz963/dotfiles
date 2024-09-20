(define-module (jd packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system linux-module)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module ((guix licenses) #:prefix license:))

(define-public xmm7360-pci
  (package
    (name "xmm7360-pci")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xmm7360/xmm7360-pci.git")
             (commit "a8ff2c6ceee84cbe74df8a78cfaa5a016d362ed4")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1aals95hk76rgd21rknqc9qfjva6g6gwicw6wz19m20bn08bs2f3"))))
    (build-system linux-module-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-makefile
            (lambda _
              (substitute* (find-files
                            "." "^Makefile(\\.include)?$")
		(("^ccflags-y := .*$")
		 "ccflags-y := -Wno-multichar -Wno-incompatible-pointer-types")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("kernel" ,linux-libre)
       ("pciutils" ,pciutils)))
    (synopsis "Kernel module for Intel XMM7360 LTE modem")
    (description
     "This package provides a kernel module for the Intel XMM7360 LTE modem, allowing it to function in Linux.")
    (home-page "https://github.com/xmm7360/xmm7360-pci")
    (license license:gpl3+)))

