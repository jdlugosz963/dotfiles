(define-module (jd packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (emacs-org-roam-ui
	    emacs-lsp-mode!))


(define emacs-org-roam-ui
  (let ((commit "9474a254390b1e42488a1801fed5826b32a8030b")
        (revision "0"))
    (package
      (name "emacs-org-roam-ui")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/org-roam/org-roam-ui")
                      (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0v54vxh95izch74wz2dl2dkdqicbvshra55l6qvd4xl5qmfhpjdc"))))
      (build-system emacs-build-system)
      (arguments
       `(#:include (cons "^out" %default-include)))
      (propagated-inputs
       (list emacs-org-roam emacs-websocket emacs-simple-httpd emacs-f))
      (home-page "https://github.com/org-roam/org-roam-ui")
      (synopsis "A graphical frontend for your org-roam Zettelkasten")
      (description " Org-Roam-UI is a frontend for exploring and interacting
with your @code{org-roam} notes. It is meant a successor of
@code{org-roam-server} that extends functionality of org-roam with a Web app
that runs side-by-side with Emacs.")
      (license license:gpl3+))))



(define emacs-lsp-mode!
  (let ((commit "808c4d0ab9f19bb92c56716cf59df89432b63f5d")
	(revision "1"))
    (package
     (inherit emacs-lsp-mode)
     (name "emacs-lsp-mode")
     (version (git-version "8.0.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-lsp/lsp-mode")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32 "0ridjhzndwjj8947vabq05njgnns74hi69x77axgcbv1c4nasz2y")))))))
