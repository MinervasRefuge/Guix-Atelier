(define-module (atelier packages duckstation)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages check)
  #:use-module (gnu packages debug)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages image)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages web)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages icu4c)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (atelier utils)
  #:use-module (ice-9 match))


;; Notes
;; xxhash is using .a still
;; duckstation before cubeb
;;git diff ../../../cubeb/ . > ~/Documents/Guix-Atelier/cubeb-duckstation.patch


;; Don't need this package for one function they added.
;; (define-public cubeb-duckstation-edition
;;   (let ((commit "54217bca3f3e0cd53c073690a23dd25d83557909")) ;; latests revision that they're using + patches :p
;;     (package
;;      (inherit cubeb)
;;      (name "cubeb-duckstation")
;;      (version (git-version "0" "1" commit))
;;      (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url "https://github.com/mozilla/cubeb")
;;                     (commit commit)))
;;               (file-name (git-file-name name version))
;;               ;; (patches (search-patches "cubeb-duckstation.patch"))
;;               (sha256 (base32 "142wix6m548sgyyrcpyl2pixcha3sm0c8j7p40879ak1q4jbhg4l")))))))



;; const char**
;; cubeb_get_backend_names()
;; {
;;   static const char* backend_names[] = {
;; #if defined(USE_PULSE)
;;     "pulse",
;; #endif
;; #if defined(USE_JACK)
;;     "jack",
;; #endif
;; #if defined(USE_ALSA)
;;     "alsa",
;; #endif
;; #if defined(USE_AUDIOUNIT)
;;     "audiounit",
;; #endif
;; #if defined(USE_WASAPI)
;;     "wasapi",
;; #endif
;; #if defined(USE_WINMM)
;;     "winmm",
;; #endif
;; #if defined(USE_SNDIO)
;;     "sndio",
;; #endif
;; #if defined(USE_SUN)
;;     "sun",
;; #endif
;; #if defined(USE_OSS)
;;     "oss",
;; #endif
;;     NULL,
;;   };

;;   return backend_names;
;; }


;; Used by libchdr but needing a 3rd party 7-zip implementation to extract the original 7-zip SDK. Do another time.
;; (define-public 7-zip
;;   (package
;;    (name "7-zip")
;;    (version "2301")
;;    (source (origin
;;             (method url-fetch)
;;             (uri (string-append "https://www.7-zip.org/a/lzma" version ".7z"))
;;             (sha256 (base32 "0w0kvl3qby1sq0i68hq10514sgfd4gl35f486i1rbzdvsqsdhz9i"))))
;;    (inputs `(,p7zip))
;;    (build-system cmake-build-system)
;;    (arguments (list #:phases
;;                 #~(modify-phases %standard-phases
;;                                 (add-before 'configure 'extract-7z
;;                                             (lambda _
;;                                               (invoke "7z" "x" "-osource" (string-append "lzma" #$version ".7z"))
;;                                               (chdir "source/C"))))))
;;    (home-page "")
;;    (synopsis "")
;;    (description "")
;;    (license license:public-domain)))

(define stb-image-resize
  ((@@ (gnu packages stb) make-stb-header-package)
   "stb-image-resize" "0.96"
   "resize images larger/smaller with good quality"))

;;todo workout the correct hash and patch since I can't track the exact version down right now.
;; (define-public libchdr
;;   (package
;;    (name "libchdr")
;;    ;(version "54bfb87")
;;    (version "2a1119c")
;;    (source (origin
;;             (method git-fetch)
;;             (uri (git-reference
;;                   (url "https://github.com/rtissera/libchdr")
;;                   (commit version)))
;;             (sha256 (base32 "1j73bk7z8vphxysihay6hjkvfi0ck624vngsd41dl0jli1y0n9bg"))
;;             (modules '((guix build utils)))
;;             (snippet `(begin
;;                         (delete-file-recursively "deps/zlib-1.3.1")
;;                         (delete-file-recursively "deps/lzma-19.00")
;;                         ;;(delete-file-recursively "deps/lzma-22.01") fix
;;                         ;;(delete-file-recursively "deps")
;;                         (substitute* "CMakeLists.txt"
;;                                      ;; (("add_subdirectory\\(deps/lzma-22\\.01 EXCLUDE_FROM_ALL\\)") "find_package(LibLZMA REQUIRED)")
;;                                      ;; (("  list\\(APPEND CHDR_LIBS lzma\\)") "")
;;                                      ;; (("  list\\(APPEND CHDR_INCLUDES lzma\\)") "")
;;                                      )))))
;;    (inputs `((,zstd "lib")
;;              ,zlib
;;              ,xz))
;;    (native-inputs
;;     (list pkg-config))
;;    (build-system cmake-build-system)
;;    (arguments (list
;;                #:tests? #f
;;                #:configure-flags '(list "-DWITH_SYSTEM_ZLIB=1" "")))
;;    (home-page "https://github.com/rtissera/libchdr")
;;    (synopsis "Standalone library for reading MAME's CHDv1-v5 formats. ")
;;    (description "libchdr is a standalone library for reading MAME's CHDv1-v5 formats.
;; The code is based off of MAME's old C codebase which read up to CHDv4 with OS-dependent 
;; features removed, and CHDv5 support backported from MAME's current C++ codebase")
;;    (license license:bsd-3)))

(define-public xbyak
  (package
   (name "xbyak")
   (version "6.73")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/herumi/xbyak")
                  (commit (string-append "v" version))))
            (sha256 (base32 "0iny9b4yikwhxgaw9phmycn1hdfvzvy4n4b10979v81vv094ib7v"))))
   (build-system cmake-build-system)
   (arguments (list
               #:phases
               '(modify-phases %standard-phases
                                (delete 'check))))
   (home-page "https://github.com/herumi/xbyak")
   (synopsis "A C++ JIT assembler for x86 (IA32), x64 (AMD64, x86-64)")
   (description "Xbyak is a C++ header library that enables dynamically to assemble x86(IA32), x64(AMD64, x86-64) mnemonic.

The pronunciation of Xbyak is kəi-bja-k. It is named from a Japanese word 開闢, which means the beginning of the world.")
   (license license:bsd-3)))

;; imgui notes
;; move "/dep/imgui/include/IconsPromptFont.h" where did this file originally come from?
;; needs patches to imgui possibly

;; Needs to be built using the repo script, not the premade version
(define-public icon-font-cpp-headers
  (let ((commit "96c8d39dee3eb33a64e42eee0d8f3ca40320df78"))
    (package
     (name "iconFontCppHeaders")
     (version (git-version "0" "1" commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/juliettef/IconFontCppHeaders")
                    (commit commit)))
              (sha256 (base32 "0lnkl0d0ramvwjv3sw61wrglsxj6aw38vg1jzx9d0iiscn8sm6cv"))))
     (build-system copy-build-system)
     (arguments (list
                 #:install-plan
                 '(list '("." "/include" #:include-regexp ("\\.h$")))))
     (home-page "")
     (synopsis "")
     (description "")
     (license #f)))) ;;fix license

;; rainterface
;; https://github.com/RetroAchievements/RAInterface
;; would need to be set up as a source only package from around the following time period
;; commit 43869bf74c878c8f45ac3ab3a0a7b5803ac4df85
;; Author: Connor McLaughlin <stenzek@gmail.com>
;; Date:   Mon Aug 22 19:33:00 2022 +1000

;;     dep: Update RAInterface

;; Glad
;; the glad files are the generated output of glad, config as follows

;; OpenGL, OpenGL ES loader generated by glad 0.1.25 on Fri Jul 20 15:42:19 2018.
;; Language/Generator: C/C++
;; Specification: gl
;; APIs: gl=4.6, gles2=3.2
;; Profile: core

(define-public zycore-c
  (package
    (name "zycore-c")
    (version "6c93d9a38e62d4db82f99bf99159e1ee78341e36") ;;todo fix, it wasn't a tagged release they used in 3.2.1 of zydis
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zyantific/zycore-c")
                    (commit "6c93d9a38e62d4db82f99bf99159e1ee78341e36")))
              (sha256 (base32 "1p3z2i8cy77q9y0i0s9g3l9wblgxps20j7phh18ci8s44nzqqf9h"))))
    (build-system cmake-build-system)
    ;;(native-inputs `(,googletest)) ;;needs CMakeLists.txt patch
    ;; (arguments (list
    ;;             #:configure-flags '(list "-DZYCORE_BUILD_TESTS=ON")))
    (arguments (list
                #:phases
                '(modify-phases %standard-phases
                   (delete 'check))))
    (home-page "https://github.com/zyantific/zycore-c")
    (synopsis "Internal library providing platform independent types, macros and a fallback for environments without LibC.")
    (description synopsis)
    (license license:expat)))

(define-public zydis-3
  (package
    (name "zydis")
    (version "3.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zyantific/zydis")
                    (commit (string-append "v" version))
                    (recursive? #t))) ;;todo fix git submodule of zycore-c (above)
              (sha256 (base32 "0ilxc21gfnh1nihw0w92abvndmkn57hl98myvx27c8vg1wcy27hl"))))
    (propagated-inputs (list zycore-c))
    (build-system cmake-build-system)
    (arguments (list
                #:phases
                '(modify-phases %standard-phases
                   (delete 'check))))
    (home-page "https://zydis.re/")
    (synopsis "Fast and lightweight x86/x86-64 disassembler library.")
    (description synopsis)
    (license license:expat)))

(define-public simpleini
  (package
    (name "simpleini")
    (version "4.22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brofield/simpleini")
                    (commit (string-append "v" version))))
              (sha256 (base32 "1hwmwnb1a3n6cza8ngr8myxsbq6hk7l328ly799wc0zpzzx7i0hz"))))
    (native-inputs `(,googletest)) ;; should use v1.14.0 of googletest currenly v1.12.1
    (build-system cmake-build-system)
    (arguments (list
                #:configure-flags
                '(list "-DSIMPLEINI_USE_SYSTEM_GTEST=ON")))
    (home-page "https://github.com/brofield/simpleini")
    (synopsis "A cross-platform library that provides a simple API to read and write INI-style configuration files.")
    (description synopsis)
    (license license:expat)))

(define-public duckstation
  (package
    (name "duckstation")
    (version "0.1-6292")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/stenzek/duckstation")
                    (commit (string-append "v" version))))
              (sha256 (base32 "090lwnzc2jllqpzyy7skd6lna7d53vdi37wd2nhnh4dh2j5szrph"))
              (patches (search-patches "duckstation.patch"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;;(delete-file-recursively "dep")
                          ;;move "/dep/imgui/include/IconsPromptFont.h" "src/util/" ;; where did this file originally come from? 
                          (for-each (λ (p) (delete-file-recursively (string-append "dep/" p)))
                                    '( "biscuit" ;;riscv
                                      "cpuinfo"
                                      "cubeb"
                                      "d3d12ma" ;;win32 related
                                      "discord-rpc"
                                      "fast_float"
                                      ;;"fmt"
                                      ;;"glad"
                                      ;;"glslang" ;; fatal error: SPIRV/GlslangToSpv.h: No such file or directory
                                      ;;"googletest"
                                      ;;"imgui" ;;requires patches
                                      ;;"libchdr" modified and unknown commit
                                      ;;"lzma"
                                      "minizip"
                                      "msvc" ;;win?
                                      ;;"rainterface"
                                      "rapidjson"
                                      ;;"rcheevos"
                                      ;;"reshadefx"
                                      "riscv-disas" ;;riscv
                                      "simpleini"
                                      ;;"soundtouch" ;; error: exception handling disabled, use ‘-fexceptions’ to enable
                                      "spirv-cross" ;; apple
                                      ;;todo fix "stb"
                                      "vixl" ;; arm32/64
                                      ;;"vulkan"
                                      "winpixeventruntime" ;;win32 related
                                      "xbyak"
                                      "xxhash"
                                      ;;"zlib"
                                      "zstd"
                                      "zydis"
                                      ))

                          ;;cubeb was modified to include a function `cubeb_get_backend_names` which returned the audio libraries built against.
                          (substitute* "src/util/cubeb_audio_stream.cpp"
                            (("const char\\*\\* cubeb_names = cubeb_get_backend_names\\(\\);")
                             (string-append "static const char* cubeb_names[] = {"
                                            (string-join '("\"alsa\"" "\"pulse\"" "nullptr") ", ") ;; todo check inputs of cubeb rather then hardcode.
                                            "};")))
                          ;;remove dep on SPIR-V (apple)
                          (substitute* "dep/reshadefx/CMakeLists.txt"
                            (("\"\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}/\\.\\./spirv-cross/include/spirv-cross\" # SPIR-V")
                             "")
                            (("src/effect_codegen_spirv\\.cpp")
                             ""))))))
    (inputs `(                  
              ,qtbase
              ,qttools
              ,qtsvg
              ,dbus
              ,curl
              ,sdl2
              ,pulseaudio  
              ,wayland
              ,libbacktrace
              ,libxrandr

              ;; Patched Deps
              ;;,soundtouch
              ;;,libchdr
              ,cubeb
              ;;,fmt
              (,zstd "lib")
              ;;,zlib ;;should be minizip?
              ,minizip
              ;;,imgui
              ;;,glslang
              ;;,glad-0.1
              ,xxhash
              ,cpuinfo
              ;;,icon-font-cpp-headers ;;part of imgui?
              ))
    (native-inputs `(,pkg-config
                     ,icu4c ;; needed for simpleini

                     ;; Patched Deps
                     ;;,stb-image-resize
                     ;;,stb-image-write
                     ;;,stb-image
                     ,simpleini
                     ,fast-float
                     ,rapidjson
                     ;; ,@(match (%current-target-system)
                     ;;     ("x86_64-linux" (list zydis-3 xbyak)))
                     ,zydis-3
                     ,xbyak
                     ))
    (build-system cmake-build-system)
    (arguments (list
                ;;#:configure-flags '(list "--debug-find")
                #:phases
                #~(modify-phases %standard-phases
                    (delete 'check)
                    (replace 'install
                      (lambda _
                        (install-file "bin/duckstation-qt"
                                      (string-append #$output "/bin"))
                        (copy-recursively "bin/resources"
                                          (string-append #$output "/bin/resources"))
                        (copy-recursively "bin/translations"
                                          (string-append #$output "/bin/translations")))))))
    (home-page "https://github.com/stenzek/duckstation")
    (synopsis "DuckStation is an simulator/emulator of the Sony PlayStation(TM) console, focusing on playability, speed, and long-term maintainability.")
    (description "DuckStation is an simulator/emulator of the Sony PlayStation(TM) console, focusing on playability, speed, and long-term maintainability. The goal is to be as accurate as possible while maintaining performance suitable for low-end devices. \"Hack\" options are discouraged, the default configuration should support all playable games with only some of the enhancements having compatibility issues.

A \"BIOS\" ROM image is required to to start the emulator and to play games. You can use an image from any hardware version or region, although mismatching game regions and BIOS regions may have compatibility issues. A ROM image is not provided with the emulator for legal reasons, you should dump this from your own console using Caetla or other means.")
    (license license:gpl3)))
