(define-module (timmy wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public seatd-timmy
  (package
    (name "seatd-timmy")
    (version "0.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~kennylevinsen/seatd")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17jaj2gxli61n1rh5175974qzs2iww188jsc3j4ac2h0fa5ldd1c"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dlibseat-builtin=enabled")))
    (native-inputs
     (list pkg-config scdoc))
    (propagated-inputs
     (list elogind))
    (home-page "https://sr.ht/~kennylevinsen/seatd")
    (synopsis "Seat management daemon and library")
    (description
     "This package provides a minimal seat management daemon whose task is to
mediate access to shared devices, such as graphics and input, for applications
that require it.  It also provides a universal seat management library that
allows applications to use whatever seat management is available.")
    (license license:expat)))

(define-public wlroots-timmy
  (package
    (name "wlroots-timmy")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/wlroots")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sshp3lvlkl1i670kxhwsb4xzxl8raz6769kqvgmxzcb63ns9ay1"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xwayland/server.c"
               (("Xwayland") (string-append (assoc-ref inputs
                                                       "xorg-server-xwayland")
                                            "/bin/Xwayland")))
             #t)))))
    (propagated-inputs
     (list ;; As required by wlroots.pc.
           eudev
           libinput
           libxkbcommon
           mesa
           pixman
           seatd-timmy
           wayland
           wayland-protocols
           xcb-util-errors
           xcb-util-wm
           xorg-server-xwayland))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/swaywm/wlroots")
    (synopsis "Pluggable, composable, unopinionated modules for building a
Wayland compositor")
    (description "wlroots is a set of pluggable, composable, unopinionated
modules for building a Wayland compositor.")
    (license license:expat)))

(define-public sway-timmy
  (package
    (name "sway-timmy")
    (version "1.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/sway")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j4sdbsrlvky1agacc0pcz9bwmaxjmrapjnzscbd2i0cria2fc5j"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hardcode path to swaybg.
             (substitute* "sway/config.c"
               (("strdup..swaybg..")
                (string-append "strdup(\"" (assoc-ref inputs "swaybg")
                               "/bin/swaybg\")")))
             ;; Hardcode path to scdoc.
             (substitute* "meson.build"
               (("scdoc.get_pkgconfig_variable..scdoc..")
                (string-append "'" (assoc-ref inputs "scdoc")
                               "/bin/scdoc'")))
             #t)))))
    (inputs (list cairo
                  elogind
                  gdk-pixbuf
                  json-c
                  libevdev
                  libinput
                  libxkbcommon
                  pango
                  swaybg
                  wayland
                  wlroots-timmy))
    (native-inputs
     (list linux-pam mesa pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/swaywm/sway")
    (synopsis "Wayland compositor compatible with i3")
    (description "Sway is a i3-compatible Wayland compositor.")
    (license license:expat)))
