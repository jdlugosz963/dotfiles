(define-module (jd utils)
  #:export (jd-search-patches))

;; This code is copied and modified from (gnu packages) module. 

(define %jd-patch-path
  (make-parameter
   (map (lambda (directory)
	  (let ((jd-patch-dir (string-append directory "/jd/packages/patches")))
            (if (and (file-exists? jd-patch-dir)
		     (file-is-directory? jd-patch-dir))
		jd-patch-dir
		directory)))
          %load-path)))

(define (jd-search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%jd-patch-path) file-name)
      (raise (string-append file-name
			    ": patch not found"))))

(define-syntax-rule (jd-search-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %JD-PATCH-PATH."
  (list (jd-search-patch file-name) ...))
