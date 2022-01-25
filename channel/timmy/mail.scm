;;
;; My versions of packages
;;

(define-module (timmy my)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages mail)
  #:use-module (ice-9 match))


(define-public mailutils-timmy
  (package
    (inherit mailutils)
    (name "mailutils-timmy")
    (arguments
     (list
           #:tests? #f
           #:configure-flags
           #~(list "--sysconfdir=/etc"
                   (string-append "--with-path-sendmail=/run/setuid-programs/sendmail")
                   ;; Add "/X.Y" to the installation directory.
                   (string-append "--with-guile-site-dir="
                                  (assoc-ref %outputs "out")
                                  "/share/guile/site/"
                                  #$(match (assoc "guile"
                                                  (package-inputs this-package))
                                      (("guile" guile)
                                       (version-major+minor
                                        (package-version guile))))))))))


