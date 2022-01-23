;;
;; My versions of packages
;;

(define-module (timmy visurf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnu-doc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (ice-9 match))

(define netsurf-buildsystem
  (package
    (name "netsurf-buildsystem")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.netsurf-browser.org/libs/releases/"
                           "buildsystem-" version ".tar.gz"))
       (sha256
        (base32
         "0alsmaig9ln8dgllb3z63gq90fiz75jz0ic71fi0k0k898qix14k"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build))))
    (home-page "https://www.netsurf-browser.org")
    (synopsis "Build system for the Netsurf project")
    (description
     "This package provides the shared build system for Netsurf project
libraries.")
    (license license:expat)))

(define netsurf-buildsystem-arguments
  `(#:make-flags `("COMPONENT_TYPE=lib-shared"
                   "CC=gcc" "BUILD_CC=gcc"
                   ,(string-append "PREFIX=" %output)
                   ,(string-append "NSSHARED="
                                   (assoc-ref %build-inputs
                                              "netsurf-buildsystem")
                                   "/share/netsurf-buildsystem"))
    #:test-target "test"
    #:phases (modify-phases %standard-phases
               (delete 'configure))))


(define-public visurf
  (package
    (name "visurf")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://git.sr.ht/~sircmpwn/visurf")
         (commit "1cd98cbfd904bb9e37f010486f027196a32391b0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1as7ips55if000j1lgslyxz3sv7mwql384k6azbi5qc4215mga5x"))))
    ;; (source
    ;;  (origin
    ;;    (method url-fetch)
    ;;    (uri (string-append "https://download.netsurf-browser.org/netsurf/"
    ;;                        "releases/source/netsurf-" version "-src.tar.gz"))
    ;;    (sha256
    ;;     (base32
    ;;      "0plra64c5xyiw12yx2q13brxsv8apmany97zqa2lcqckw4ll8j1n"))
    ;;    (patches (search-patches "netsurf-system-utf8proc.patch"
    ;;                             "netsurf-y2038-tests.patch"
    ;;                             "netsurf-longer-test-timeout.patch"
    ;;                             "netsurf-message-timestamp.patch"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list
      netsurf-buildsystem
      nsgenbind
      libidn
      check
      perl
      perl-html-parser
      pkg-config
      xxd))
    (inputs
     (list
      curl
      gtk+
      openssl
      utf8proc
      libpng
      libjpeg-turbo
      libcss
      libdom
      libnsbmp
      libnsgif
      libnslog
      libnspsl
      libnsutils
      libsvgtiny
      miscfiles))
    (arguments
     `(#:make-flags `("CC=gcc" "BUILD_CC=gcc"
                      "TARGET=visurf"
                      ,(string-append "PREFIX=" %output)
                      ,(string-append "NSSHARED="
                                      (assoc-ref %build-inputs
                                                 "netsurf-buildsystem")
                                      "/share/netsurf-buildsystem"))
       #:test-target "test"
       #:modules ((ice-9 rdelim)
                  (ice-9 match)
                  (srfi srfi-1)
                  (sxml simple)
                  ,@%glib-or-gtk-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'patch-check
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("test/bloom.c" "test/hashtable.c")
               (("/usr/share/dict/words")
                (string-append (assoc-ref inputs "miscfiles") "/share/web2")))
             #t)))))
    (home-page "https://git.sr.ht/~sircmpwn/visurf")
    (synopsis "Web browser")
    (description
     "visurf is a work-in-progress Wayland-based frontend for NetSurf. NetSurf is a
lightweight web browser that has its own layout and rendering engine entirely
written from scratch.  It is small and capable of handling many of the web
standards in use today.")
    (license license:gpl2+)))
