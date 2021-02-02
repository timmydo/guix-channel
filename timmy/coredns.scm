(define-module (timmy coredns)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:))

(define-public coredns
  (let ((commit "78de01a9cddf140c04ec3c4095195177d21cacff")
	(revision "0"))
    (package
     (name "coredns")
     (version (git-version "1.8.1" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/timmydo/coredns")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
	       base32 "02gdj866mz17p1f0fgfjpbb9cah2ykziacahpkw0viq1vj231hai")))
     (build-system go-build-system)
     (arguments
      '(#:import-path "github.com/timmydo/coredns"))
     (synopsis "DNS server/forwarder, written in Go, that chains plugins")
     (description "CoreDNS is a fast and flexible DNS server.
  The key word here is flexible: with CoreDNS you are able to do
what you want with your DNS data by utilizing plugins.  If some
functionality is not provided out of the box you can add it by
writing a plugin.")
     (home-page "https://github.com/coredns/coredns")
     (license license:asl2.0))))
