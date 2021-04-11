(define-module (timmy zig)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)

  ;; for package
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages llvm)
  #:use-module (ice-9 match)

)

(define-public zig
  (let ((commit "783cb980ab137d6a99c20f007d70b4191d4a63c4")
        (revision "0")
        (version "0.71"))
    (package
      (name "zig")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ziglang/zig.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1z6c4ym9jmga46cw2arn7zv2drcpmrf3vw139gscxp27n7q2z5md"))))
      (build-system cmake-build-system)
      (native-inputs `(("lld@11.0.0" ,lld)
                       ("clang-toolchain-11" ,clang-toolchain-11)))
      (arguments
       `(#:tests? #f ;;; there isn't a test target
         #:phases
         (modify-phases %standard-phases
           (add-before 'check 'check-setup
             (lambda _
               ;; https://github.com/ziglang/zig/issues/6810
               (setenv "XDG_CACHE_HOME" "/tmp/xdg-cache")
               #t)))))
      (synopsis "General purpose programming language and toolchain")
      (description "Zig is a general-purpose programming language and
toolchain for maintaining robust, optimal, and reusable software.")
      (home-page "https://github.com/ziglang/zig")
      (license
       (list
        license:expat)))))

zig
