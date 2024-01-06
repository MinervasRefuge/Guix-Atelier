(define-module (atelier packages polymc)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages java)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages audio))

;; (define-public nbtplusplus
;;   (package
;;    (name "")
;;    (version "")
;;    (source (origin
;;             (method git-fetch)
;;             (uri (git-reference
;;                   (url "")
;;                   ()))))))

;;todo extract submodules
;;https://github.com/gulrak/filesystem
;;https://github.com/PolyMC/libnbtplusplus.git
;;https://github.com/stachenov/quazip.git
;;https://github.com/marzer/tomlplusplus.git

(define glfw-with-ld
  (package
   (inherit glfw)
   (native-search-paths
    (list (search-path-specification
           (variable "LD_LIBRARY_PATH")
           (files '("lib")))))))

(define openal-with-ld
  (package
   (inherit openal)
   (native-search-paths
    (list (search-path-specification
           (variable "LD_LIBRARY_PATH")
           (files '("lib")))))))

;; Note don't use this outside of a shell. It could break a few programs.
;;
;; Must use native shared libraries. Unfortunetly it's a pain (as in I haven't found an alternative to pass it through to the java
;; other then through LD_LIBRARY_PATH.
;; nix uses a wrapper "postInstall" (?) to add the natives to LD path. It looks like to patch the source you could adjust the task objects
;; which have a m_process member (QProcess) and in the task constructors (derive from QThread) you can find the env call setup.
;;
;; QProcessEnvironment object is probably where you could insert LD_LIBRARY_PATH so the java instance can search the right paths.
;; Not a great method. What about -Djava.library.path ? it hasn't worked (possibly overridden?)
;; It's set a seccond time, so getting overriten. It could also be patched around this, but I haven't tracked it down the right line yet
(define-public polymc
  (package
   (name "polymc")
   (version "6.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/PolyMC/PolyMC")
           (recursive? #t)
           (commit version)))
     (file-name (git-file-name name version))
     (sha256 (base32 "07xg1971ycp7mzvcglwpgzl0gc1bxrc5vx32c63nkl2cdn1n582r"))))
   (inputs (list qtbase-5
                 qtcharts
                 extra-cmake-modules
                 zlib
                 glfw
                 openal))
   (propagated-inputs `((,openjdk18 "jdk")
                        ,openjdk18
                        ,openal-with-ld
                        ,glfw-with-ld))
   (build-system cmake-build-system)
   (arguments (list
               #:configure-flags
               #~(list "-DENABLE_LTO=ON"
                       ;; "-DLINKER_OPTIONS=\"-Wl,-rpath,/gnu/store/jgyyrj020p8wl6cazabmaprw2dc0kzrb-openal-1.22.2/lib:/gnu/store/wkk188nmyxfacm2g605j830wascbpk0y-glfw-3.3.4/lib/\""
                       )
               #:tests? #f
               #:build-type "Release"))
   (home-page "https://polymc.org/")
   (synopsis "An Open Source Minecraft launcher")
   (description "An Open Source Minecraft launcher with the ability to manage multiple instances, accounts and mods. Focused on user freedom and free redistributability.")
   (license license:gpl3+)))
