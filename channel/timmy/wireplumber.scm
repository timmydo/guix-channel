(define-module (timmy wireplumber)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages web)
  #:use-module (ice-9 match))

(define-public wireplumber-timmy
  (package
    (name "wireplumber-timmy")
    (version "0.4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/pipewire/wireplumber")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vr84xlnwlhrfq9l13rxh8cb8s389wlyafvi6bah444acmpx1lwv"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags (list "-Dsystem-lua=true" "-Delogind=disabled")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib-bin" ,glib "bin")))
    (inputs
     `(("dbus" ,dbus)
       ("glib" ,glib)
       ("lua" ,lua)
       ("pipewire" ,pipewire-0.3)))
    (home-page "https://pipewire.org/")
    (synopsis "Modular session and policy manager for PipeWire")
    (description "WirePlumber is a daemon that links streams from applications to their
appropriate device.  For example WirePlumber could link an audio stream to a
speaker.")
    (license license:expat)))
