(define-module (jd utils)
  #:use-module (gnu system)
  #:use-module (ice-9 match)
  #:export (jd-search-patches
	    current-operating-system
	    current-home))

(define primitive-host-operating-system
  "/etc/config.scm")

(define-macro (define-combine-operating-systems name fields)
  `(define (,name os-to-inherit-from os)
     (operating-system
      (inherit os-to-inherit-from)
      ,@(map (lambda (field)
	       `(,field
		 (,(string->symbol
		    (string-append
		     "operating-system-"
		     (symbol->string field)))
		  os)))
	     fields))))

(define-combine-operating-systems combine-install-dest-informations
  (bootloader mapped-devices file-systems))

(define (make-reconfigureable-os os)
  (combine-install-dest-informations
   os
   (load primitive-host-operating-system)))

(define (current-operating-system)
  (make-reconfigureable-os
   (module-ref
    (resolve-module
     `(jd desktops ,(string->symbol (gethostname))))
    'system)))

(define (current-home)
  (module-ref
   (resolve-module
    `(jd desktops ,(string->symbol (gethostname))))
   'home))

;; This code is copied and modified from (gnu packages) module. 

(define (make-custom-load-path dir-path)
  (make-parameter
   (map (lambda (directory)
	  (let ((custom-dir (string-append directory dir-path)))
            (if (and (file-exists? custom-dir)
		     (file-is-directory? custom-dir))
		custom-dir
		directory)))
        %load-path)))

(define (make-custom-searcher load-path)
  (lambda (file-name)
    (or (search-path (load-path) file-name)
	(raise (string-append file-name
			      ": not found")))))

(define %jd-patch-path (make-custom-load-path "/jd/packages/patches"))
(define %jd-dot-files-path (make-custom-load-path "/jd/home/services/dotfiles"))

(define (jd-search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%jd-patch-path) file-name)
      (raise (string-append file-name
			    ": patch not found"))))

(define-syntax-rule (jd-search-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %JD-PATCH-PATH."
  (list (jd-search-patch file-name) ...))
