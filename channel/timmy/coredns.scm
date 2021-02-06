(define-module (timmy coredns)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix packages)
  #:use-module (guix records)

  #:use-module (guix gexp)
  #:use-module (guix git-download)  
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-1)
  
  #:export (
            coredns-configuration-content
            coredns-service
            coredns-service-type
            coredns))

(define-record-type* <coredns-configuration>
  coredns-configuration make-coredns-configuration
  coredns-configuration?
  (package coredns-configuration-package
           (default coredns))
  (name coredns-configuration-name (default "default"))
  (config-file coredns-configuration-config-file
               (default "/etc/Corefile")))

(define %coredns-accounts
  (list (user-group (name "coredns") (system? #t))
        (user-account
         (name "coredns")
         (group "coredns")
         (system? #t)
         (comment "coredns server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (coredns-shepherd-service config)
  (match-record config
      <coredns-configuration>
    (package name config-file)
    (let ((pid-file (string-append "/var/coredns/" name "/_.pid"))
          (coredns-bin (file-append package "/bin/coredns")))
      (list (shepherd-service
             (provision '(coredns))
             (documentation "Run the coredns daemon.")
             (requirement '(networking))
             (start #~(make-forkexec-constructor
                       `(#$coredns-bin
                         #$@(list "-pi" pid-file)
                         #$@(list "-conf" config-file))
                       #:pid-file #$pid-file))
             (stop #~(make-kill-destructor)))))))

(define coredns-service-type
  (service-type
   (name 'coredns)
   (extensions
    (list (service-extension shepherd-root-service-type
                             coredns-shepherd-service)
          (service-extension account-service-type
                             (const %coredns-accounts))))
   (compose concatenate)
   (default-value (coredns-configuration))
   (description "Run the coredns Web server.")))


(define coredns
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
	       (base32 "02gdj866mz17p1f0fgfjpbb9cah2ykziacahpkw0viq1vj231hai"))))
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

coredns
