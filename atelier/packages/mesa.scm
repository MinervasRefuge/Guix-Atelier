(define-module (atelier packages mesa)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip)))

(define-public mesa-with-i915
  (package
   (inherit mesa)
   (name "mesa-with-i915")
   (arguments
    (list
     #:configure-flags
     #~(list
        #$@(match (%current-system)
             ("aarch64-linux"
              ;; TODO: Fix svga driver for non-Intel architectures.
              '("-Dgallium-drivers=etnaviv,freedreno,kmsro,lima,nouveau,panfrost,r300,r600,swrast,tegra,v3d,vc4,virgl"))
             ("armhf-linux"
              ;; Freedreno FTBFS when built on a 64-bit machine.
              '("-Dgallium-drivers=etnaviv,kmsro,lima,nouveau,panfrost,r300,r600,swrast,tegra,v3d,vc4,virgl"))
             ((or "powerpc64le-linux" "powerpc-linux" "riscv64-linux")
              '("-Dgallium-drivers=nouveau,r300,r600,radeonsi,swrast,virgl"))
             (_
              '("-Dgallium-drivers=crocus,iris,nouveau,r300,r600,radeonsi,svga,swrast,virgl,i915")))
        ;; Enable various optional features.  TODO: opencl requires libclc,
        ;; omx requires libomxil-bellagio
        "-Dplatforms=x11,wayland"
        "-Dglx=dri"        ;Thread Local Storage, improves performance
        ;; "-Dopencl=true"
        ;; "-Domx=true"
        "-Dosmesa=true"
        "-Dgallium-xa=enabled"
        ;; "-Dgallium-va=enabled" ;libva? 

        ;; features required by wayland
        "-Dgles2=enabled"
        "-Dgbm=enabled"
        "-Dshared-glapi=enabled"

        ;; Explicitly enable Vulkan on some architectures.
        #$@(match (%current-system)
             ((or "i686-linux" "x86_64-linux")
              '("-Dvulkan-drivers=intel,amd"))
             ((or "powerpc64le-linux" "powerpc-linux")
              '("-Dvulkan-drivers=amd,swrast"))
             ("aarch64-linux"
              '("-Dvulkan-drivers=freedreno,amd,broadcom,swrast"))
             ("riscv64-linux"
              '("-Dvulkan-drivers=amd,swrast"))
             (_
              '("-Dvulkan-drivers=auto")))

        ;; Enable the Vulkan overlay layer on all architectures.
        "-Dvulkan-layers=device-select,overlay"

        ;; Enable the codecs that were built by default as part of the
        ;; 21.3.x releases to avoid functionality regressions.
        "-Dvideo-codecs=vc1dec,h264dec,h264enc,h265dec,h265enc"

        ;; Also enable the tests.
        "-Dbuild-tests=true"

        "-Dllvm=enabled")               ; default is x86/x86_64 only

     ;; XXX: 'debugoptimized' causes LTO link failures on some drivers.  The
     ;; documentation recommends using 'release' for performance anyway.
     #:build-type "release"

     #:modules '((ice-9 match)
                 (srfi srfi-1)
                 (guix build utils)
                 (guix build meson-build-system))
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-test
           (lambda _
             ;; Disable the intel vulkan (anv_state_pool) tests, as they may
             ;; fail in a nondeterministic fashion (see:
             ;; https://gitlab.freedesktop.org/mesa/mesa/-/issues/5446).
             (substitute* "src/intel/vulkan/meson.build"
               (("if with_tests")
                "if false"))
             #$@(match (%current-system)
                  ("riscv64-linux"
                   ;; According to the test logs the llvm JIT is not designed
                   ;; for this architecture and the llvmpipe tests all segfault.
                   ;; The same is true for mesa:gallium / osmesa-render.
                   `((substitute* '("src/gallium/drivers/llvmpipe/meson.build"
                                    "src/gallium/targets/osmesa/meson.build")
                       (("if with_tests") "if false"))))
                  ("powerpc64le-linux"
                   ;; Disable some of the llvmpipe tests.
                   `((substitute* "src/gallium/drivers/llvmpipe/lp_test_arit.c"
                       (("0\\.5, ") ""))))
                  ("powerpc-linux"
                   ;; There are some tests which fail specifically on powerpc.
                   `((substitute* '( ;; LLVM ERROR: Relocation type not implemented yet!
                                    "src/gallium/drivers/llvmpipe/meson.build"
                                    ;; This is probably a big-endian test failure.
                                    "src/gallium/targets/osmesa/meson.build")
                       (("if with_tests") "if not with_tests"))
                     (substitute* "src/util/tests/format/meson.build"
                       ;; This is definately an endian-ness test failure.
                       (("'u_format_test', ") ""))
                     ;; It is only this portion of the test which fails.
                     (substitute* "src/mesa/main/tests/meson.build"
                       ((".*mesa_formats.*") ""))
                     ;; This test times out and receives SIGTERM.
                     (substitute* "src/amd/common/meson.build"
                       (("and not with_platform_windows") "and with_platform_windows"))))
                  ("i686-linux"
                   ;; This test is known to fail on i686 (see:
                   ;; https://gitlab.freedesktop.org/mesa/mesa/-/issues/4091).
                   `((substitute* "src/util/meson.build"
                       ((".*'tests/u_debug_stack_test.cpp',.*") ""))))
                  ("aarch64-linux"
                   ;; The ir3_disasm test segfaults.
                   ;; The simplest way to skip it is to run a different test instead.
                   `((substitute* "src/freedreno/ir3/meson.build"
                       (("disasm\\.c'") "delay.c',\n    link_args: ld_args_build_id"))))
                  ("armhf-linux"
                   ;; Disable some of the llvmpipe tests.
                   `((substitute* "src/gallium/drivers/llvmpipe/meson.build"
                       (("'lp_test_arit', ") ""))))
                  (_
                   '((display "No tests to disable on this architecture.\n"))))))
         (add-before 'configure 'fix-dlopen-libnames
           (lambda _
             (let ((out #$output))
               ;; Remain agnostic to .so.X.Y.Z versions while doing
               ;; the substitutions so we're future-safe.
               (substitute* "src/glx/meson.build"
                 (("-DGL_LIB_NAME=\"lib@0@\\.so\\.@1@\"")
                  (string-append "-DGL_LIB_NAME=\"" out
                                 "/lib/lib@0@.so.@1@\"")))
               (substitute* "src/gbm/backends/dri/gbm_dri.c"
                 (("\"libglapi\\.so")
                  (string-append "\"" out "/lib/libglapi.so")))
               (substitute* "src/gbm/main/backend.c"
                 ;; No need to patch the gbm_gallium_drm.so reference;
                 ;; it's never installed since Mesa removed its
                 ;; egl_gallium support.
                 (("\"gbm_dri\\.so")
                  (string-append "\"" out "/lib/dri/gbm_dri.so"))))))
         (add-after 'install 'split-outputs
           (lambda _
             (let ((out #$output)
                   (bin #$output:bin))
               ;; Not all architectures have the Vulkan overlay control script.
               (mkdir-p (string-append out "/bin"))
               (call-with-output-file (string-append out "/bin/.empty")
                 (const #t))
               (copy-recursively (string-append out "/bin")
                                 (string-append bin "/bin"))
               (delete-file-recursively (string-append out "/bin")))))
         (add-after 'install 'symlinks-instead-of-hard-links
           (lambda _
             ;; All the drivers and gallium targets create hard links upon
             ;; installation (search for "hardlink each megadriver instance"
             ;; in the makefiles).  This is no good for us since we'd produce
             ;; nars that contain several copies of these files.  Thus, turn
             ;; them into symlinks, which saves ~124 MiB.
             (let* ((out    #$output)
                    (lib    (string-append out "/lib"))
                    (files  (find-files lib
                                        (lambda (file stat)
                                          (and (string-contains file ".so")
                                               (eq? 'regular
                                                    (stat:type stat))))))
                    (inodes (map (compose stat:ino stat) files)))
               (for-each (lambda (inode)
                           (match (filter-map (match-lambda
                                                ((file ino)
                                                 (and (= ino inode) file)))
                                              (zip files inodes))
                             ((_)
                              #f)
                             ((reference others ..1)
                              (format #t "creating ~a symlinks to '~a'~%"
                                      (length others) reference)
                              (for-each delete-file others)
                              (for-each (lambda (file)
                                          (if (string=? (dirname file)
                                                        (dirname reference))
                                              (symlink (basename reference)
                                                       file)
                                              (symlink reference file)))
                                        others))))
                         (delete-duplicates inodes)))))
         (add-after 'install 'set-layer-path-in-manifests
           (lambda _
             (let* ((out #$output)
                    (implicit-path (string-append
                                    out
                                    "/share/vulkan/implicit_layer.d/"))
                    (explicit-path (string-append
                                    out
                                    "/share/vulkan/explicit_layer.d/"))
                    (fix-layer-path
                     (lambda (layer-name)
                       (let* ((explicit (string-append explicit-path layer-name ".json"))
                              (implicit (string-append implicit-path layer-name ".json"))
                              (manifest (if (file-exists? explicit)
                                            explicit
                                            implicit)))
                         (substitute* manifest
                           (((string-append "\"lib" layer-name ".so\""))
                            (string-append "\"" out "/lib/lib" layer-name ".so\"")))))))
               (for-each fix-layer-path '("VkLayer_MESA_device_select"
                                          "VkLayer_MESA_overlay"))))))))))
