(define-module (atelier packages duckstation)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages debug)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages freedesktop)
  #:use-module ((guix licenses) #:prefix license:))

(define-public duckstation
  (package
   (name "duckstation")
   (version "0.1-6292")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/stenzek/duckstation")
                  (commit (string-append "v" version))))
            (sha256 (base32 "090lwnzc2jllqpzyy7skd6lna7d53vdi37wd2nhnh4dh2j5szrph"))))
   (inputs (list
            pkg-config
            extra-cmake-modules
            qtbase
            qttools
            qtsvg
            dbus
            curl
            sdl2
            wayland
            qtwayland
            libbacktrace
            libxrandr))
   (build-system cmake-build-system)
   (arguments (list
               #:phases
               #~(modify-phases %standard-phases
                                (add-before 'configure 'qt-cmake-patch
                                            (lambda _
                                              (substitute* "CMakeLists.txt"
                                                           (("  find_package\\(Qt6 6.5.3")
                                                            "  find_package(Qt6 6.5.2"))))
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
