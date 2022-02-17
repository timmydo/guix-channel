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


(define-public libcss-git
  (package
    (name "libcss")
    (version "0.9.1.1")
 
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "git://git.netsurf-browser.org/libcss.git")
         (commit "accad499aed29acb7bc8fb00bea3d9f2b7f43bd1")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mc604r2mkc080yxx1kaw5jwxjslz4c52ph5vnwdkwh0ywrwc8s8"))))

    (arguments
     `(#:make-flags `("COMPONENT_TYPE=lib-shared"
                      "CC=gcc" "BUILD_CC=gcc"
                      ,(string-append "PREFIX=" %output)
                      ,(string-append "NSSHARED="
                                      (assoc-ref %build-inputs
                                                 "netsurf-buildsystem")
                                      "/share/netsurf-buildsystem"))
    #:test-target "test"
    #:phases (modify-phases %standard-phases
               (delete 'configure)
               )))
    
    (build-system gnu-build-system)
    (native-inputs
     (list netsurf-buildsystem pkg-config perl utf8proc))
    (propagated-inputs                  ;needed for libcss.pc
     (list libparserutils libwapcaplet))
    (home-page "https://www.netsurf-browser.org/projects/libcss/")
    (synopsis "CSS parser and selection library")
    (description
     "LibCSS is a CSS (Cascading Style Sheet) parser and selection engine,
written in C.  It is developed as part of the NetSurf project.")
    (license license:expat)))

(define-public visurf
  (package
    (name "visurf")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://git.sr.ht/~sircmpwn/visurf")
         (commit "7de863576fbfb0a23d295b22e2c8739b5d1dac57")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ck3xcmsvxm7hzvmhlsri74nm5c3iki7w9gaqyqvrkg6m0i0zwys"))))
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
      libcss-git
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
             #t))

         (add-after 'unpack 'patch-utf8
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("utils/idna.c")
               (("libutf8proc/utf8proc.h") "utf8proc.h"))
             #t)))))

    
    (home-page "https://git.sr.ht/~sircmpwn/visurf")
    (synopsis "Web browser")
    (description
     "visurf is a work-in-progress Wayland-based frontend for NetSurf. NetSurf is a
lightweight web browser that has its own layout and rendering engine entirely
written from scratch.  It is small and capable of handling many of the web
standards in use today.")
    (license license:gpl2+)))

visurf
