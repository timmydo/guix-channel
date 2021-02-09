(define-module (timmy radvd)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages pkg-config)
  #:use-module (ice-9 match)

  #:export (
            radvd-configuration
            radvd-service
            radvd-service-type))

(define-public radvd
  (let ((commit "f85392a68c7cd0fe5525b4218be07b893402b69b")
        (revision "0")
        (version "2.19"))
    (package
      (name "radvd")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/radvd-project/radvd.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1df827m3vkjq2bcs5y9wg2cygvpdwl8ppl446qqhyym584gz54nl"))))
      (build-system gnu-build-system)
      (native-inputs `(("autoconf" ,autoconf)
                       ("automake" ,automake)
                       ("bison" ,bison)
                       ("check" ,check)
                       ("flex" ,flex)                       
                       ("libtool" ,libtool)
                       ("pkg-config" ,pkg-config)))
      (arguments
       `(#:tests? #f)) ; -lcheck not being added to LIBS in `make check`
      (synopsis "Routing advertisement daemon")
      (description "radvd sends routing advertisements which are used by hosts
to configure their IPv6 interfaces")
      (home-page "https://github.com/radvd-project/radvd")
      (license
       (list
        license:bsd-4)))))

(define-record-type* <radvd-configuration>
  radvd-configuration make-radvd-configuration
  radvd-configuration?
  (package radvd-configuration-package
           (default radvd))
  (name radvd-configuration-name (default "default"))
  (config-file radvd-configuration-config-file
               (default "/etc/radvd.conf")))

(define %radvd-accounts
  (list (user-group (name "radvd") (system? #t))
        (user-account
         (name "radvd")
         (group "radvd")
         (system? #t)
         (comment "radvd server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (radvd-shepherd-service config)
  (match-record config
      <radvd-configuration>
    (package name port config-file)
    (let ((radvd-bin (file-append package "/sbin/radvd"))
          (log-filename (string-append "/var/log/radvd-" name ".log")))
      (list (shepherd-service
             (provision '(radvd))
             (documentation "Run the radvd daemon.")
             (requirement '(networking))
             (start #~(make-forkexec-constructor
                       `(#$radvd-bin
                         #$@(list "-C" config-file)
                         #$@(list "-n" "-m" "stderr"))
                       #:user "radvd" #:group "radvd"
                       #:log-file #$log-filename))
             (stop #~(make-kill-destructor)))))))

(define radvd-service-type
  (service-type
   (name 'radvd)
   (extensions
    (list (service-extension shepherd-root-service-type
                             radvd-shepherd-service)
          (service-extension account-service-type
                             (const %radvd-accounts))))
   (compose concatenate)
   (default-value (radvd-configuration))
   (description "Run the radvd.")))


