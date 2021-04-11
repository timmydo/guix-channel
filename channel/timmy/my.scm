;;
;; My versions of packages
;;

(define-module (timmy my)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (ice-9 match))


(define-public my-hello
  (package
    (inherit hello)
    (name "my-hello")
    (arguments
     `(#:make-flags
       (list "CFLAGS=-g -fno-omit-frame-pointer -O3 -march=native")))))
