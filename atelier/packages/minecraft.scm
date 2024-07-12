(define-module (atelier packages minecraft)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages java)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

;; (define glfw-with-ld
;;   (package
;;    (inherit glfw)
;;    (native-search-paths
;;     (list (search-path-specification
;;            (variable "LD_LIBRARY_PATH")
;;            (files '("lib")))))))

;; (define glfw-3.4
;;   (package
;;    (inherit glfw)
;;    (name "glfw")
;;    (version "3.4")
;;    (source (origin
;;             (method url-fetch)
;;             (uri (string-append "https://github.com/glfw/glfw"
;;                                 "/releases/download/" version
;;                                 "/glfw-" version ".zip"))
;;             (sha256 (base32 "1sd396kkn53myp61kxrd18h7b1q4ix173hhxhvl0iz8j4x5h1v5m"))))
;;    (arguments
;;     (substitute-keyword-arguments (package-arguments glfw)
;;                                   ((#:configure-flags original-flags #~(list))
;;                                    #~(append #$original-flags (list
;;                                                                "-DGLFW_BUILD_DOCS=OFF" ;;mimics nixs build of this package
;;                                                                (string-append "-DCMAKE_C_FLAGS=-D_GLFW_GLX_LIBRARY='\"" #$mesa "/lib/libGL.so\"'")
;;                                                                (string-append "-DCMAKE_C_FLAGS=-D_GLFW_EGL_LIBRARY='\"" #$mesa "/lib/libEGL.so\"'"))))))
;;    (native-inputs (append (package-native-inputs glfw) `(("pkgconf" ,pkgconf))))))

;; (define glfw-3.4-with-ld
;;   (package
;;    (inherit glfw-3.4)
;;    (native-search-paths
;;     (list (search-path-specification
;;            (variable "LD_LIBRARY_PATH")
;;            (files '("lib")))))))

;; Debug Notes
;;-Djava.library.path=""
;;-Dorg.lwjgl.librarypath=""
;;-Xlog:library=info
;;-Dorg.lwjgl.util.Debug=true
;;-Dorg.lwjgl.util.DebugLoader=true

;; Use the system provided /openal/ and the /lwjgl/ provided /glfw/.
(define-public prism-launcher
  (package
   (name "prism-launcher")
   (version "8.4")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/PrismLauncher/PrismLauncher")
                  (recursive? #t) ;; todo fix
                  (commit version)))
            (sha256 (base32 "07ngh55rqxslrs3q1qlydxavxcc39dmxsgqnlv7xmn13ci1n5vsr"))))
   (build-system cmake-build-system)
   (arguments (list
               ;; LTO get's stuck towards the end of compiling.
               ;; #:configure-flags
               ;; '(list  "-DENABLE_LTO=ON")
               #:tests? #f
               #:build-type "Release"
               #:phases
               #~(modify-phases
                  %standard-phases
                  (add-after 'install 'wrap-program
                             (λ _ (wrap-program (string-append #$output "/bin/prismlauncher")
                                                `("LD_LIBRARY_PATH" prefix ,(map (λ (pkg-path) (string-append pkg-path "/lib"))
                                                                                 '(#$mesa
                                                                                   #$libx11
                                                                                   ;;#$glfw-3.4
                                                                                   #$openal)))
                                                `("PRISMLAUNCHER_JAVA_PATHS" = (,(string-append #$openjdk18:jdk "/bin/java")))))))))
   (inputs `(,qtbase
             ,qt5compat
             ,zlib
             ;; needed at run-time
             ;;,glfw-3.4
             ,openal
             ,mesa
             ,libx11
             (,openjdk18 "jdk")))
   (native-inputs `(,extra-cmake-modules))
   (synopsis "Prism Launcher is a custom launcher for Minecraft that allows you to easily manage multiple installations of Minecraft at once.")
   (description synopsis)
   (home-page "https://github.com/PrismLauncher/PrismLauncher")
   (license license:gpl3)))

;; For what ever reason the native glfw won't work and raises an exception. Without writing a small tester using /lwjgl/ I'm not sure it will be
;; found. I know when `glfw-3.4-with-ld` is used as a `propagated-inputs` it does work, which tells me it's something to do with the environment.
;; I haven't traced it down. Matching the same lib visible in said shell env via `LD_LIBRARY_PATH` doesn't change the outcome. Note that Prism
;; does strip /some/ paths from `LD_LIBRARY_PATH` when spawning `jdk`.
;;
;; Might be possible to inherit/extend/input and repackage to wrap (via `wrap-program`) the Java bins an environment regardless of what Prism
;; chooses. ¯\_(ツ)_/¯
