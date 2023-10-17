(define-module (atelier packages rust)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages web)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python))

(define-public libgit2
  (package
    (name "libgit2")
    (version "1.7.1")
    (source (origin
              ;; Since v1.1.1, release artifacts are no longer offered (see:
              ;; https://github.com/libgit2/libgit2/discussions/5932#discussioncomment-1682729).
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libgit2/libgit2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wq6a91k97gbsyafla39yvn1lnr559hqc41ksz1qxv7flf5kyvfx"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "deps/chromium-zlib")
		  (delete-file-recursively "deps/http-parser")
		  (delete-file-recursively "deps/ntlmclient")
		  (delete-file-recursively "deps/pcre")
		  (delete-file-recursively "deps/winhttp")
		  (delete-file-recursively "deps/zlib"))))) ;; leaving in xdiff
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags
       (list "-DUSE_NTLMCLIENT=OFF" ;TODO: package this
             "-DREGEX_BACKEND=pcre2"
             "-DUSE_HTTP_PARSER=system"
             "-DUSE_SSH=ON" ; cmake fails to find libssh if this is missing
             ,@(if (%current-target-system)
                   `((string-append
                      "-DPKG_CONFIG_EXECUTABLE="
                      (search-input-file
                       %build-inputs
                       (string-append "/bin/" ,(%current-target-system)
                                      "-pkg-config"))))
                   '()))
       #:phases
       (modify-phases %standard-phases
         ,@(if (target-arm32?)
             ;; Some tests are flaky on armhf.
             '((add-before 'check 'pre-check
                 (lambda _
                   (setenv "GITTEST_FLAKY_STAT" "true"))))
             '())
         ;; Run checks more verbosely, unless we are cross-compiling.
         (replace 'check
           (lambda* (#:key (tests? #t) #:allow-other-keys)
             (if tests?
                 (invoke "./libgit2_tests" "-v" "-Q")
                 ;; Tests may be disabled if cross-compiling.
                 (format #t "Test suite not run.~%")))))))
    (inputs
     (list libssh2 http-parser))
    (native-inputs
     (list pkg-config python))
    (propagated-inputs
     ;; These libraries are in 'Requires.private' in libgit2.pc.
     (list openssl pcre2 zlib))
    (home-page "https://libgit2.org/")
    (synopsis "Library providing Git core methods")
    (description
     "Libgit2 is a portable, pure C implementation of the Git core methods
provided as a re-entrant linkable library with a solid API, allowing you to
write native speed custom Git applications in any language with bindings.")
    ;; GPLv2 with linking exception
    (license license:gpl2)))

(define-public rust-bitflags-2
  (package
   (name "rust-bitflags")
   (version "2.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "bitflags" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1cxcbx6mm1w2d7x9xd2vcqgr69a2ydw9k110ir7rrkmmrdwyn2y7"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                      ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                      ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                      ("rust-serde" ,rust-serde-1))
      #:cargo-development-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                                  ("rust-rustversion" ,rust-rustversion-1)
                                  ("rust-serde-derive" ,rust-serde-derive-1)
                                  ("rust-serde-json" ,rust-serde-json-1)
                                  ("rust-serde-test" ,rust-serde-test-1)
                                  ("rust-trybuild" ,rust-trybuild-1))))
   (home-page "https://github.com/bitflags/bitflags")
   (synopsis "A macro to generate structures which behave like bitflags.")
   (description
    "This package provides a macro to generate structures which behave like bitflags.")
   (license (list license:expat license:asl2.0))))

(define-public rust-phf-macros-0.11
  (package
    (name "rust-phf-macros")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "phf_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0js61lc0bhzzrbd9vhpcqp11vvwckdkz3g7k95z5h1k651p68i1l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-phf-generator" ,rust-phf-generator-0.11)
                       ("rust-phf-shared" ,rust-phf-shared-0.11)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-unicase" ,rust-unicase-2))))
    (home-page "https://github.com/rust-phf/rust-phf")
    (synopsis "Macros to generate types in the phf crate")
    (description "Macros to generate types in the phf crate")
    (license license:expat)))

(define-public rust-phf-shared-0.11
  (package
    (name "rust-phf-shared")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "phf_shared" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0azphb0a330ypqx3qvyffal5saqnks0xvl8rj73jlk3qxxgbkz4h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-siphasher" ,rust-siphasher-0.3)
                       ("rust-uncased" ,rust-uncased-0.9)
                       ("rust-unicase" ,rust-unicase-2))))
    (home-page "https://github.com/rust-phf/rust-phf")
    (synopsis "Support code shared by PHF libraries")
    (description "Support code shared by PHF libraries")
    (license license:expat)))

(define-public rust-phf-0.11.2
  (package
   (name "rust-phf")
   (version "0.11.2")
   (source (origin
            (method url-fetch)
            (uri (crate-uri "phf" version))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "1p03rsw66l7naqhpgr1a34r9yzi1gv9jh16g3fsk6wrwyfwdiqmd"))))
   (build-system cargo-build-system)
   (arguments
    `(#:tests? #f			; Doc tests fail.
      #:cargo-inputs
      (("rust-phf-macros" ,rust-phf-macros-0.11)
       ("rust-phf-shared" ,rust-phf-shared-0.11)
       ("rust-serde" ,rust-serde-1))))
   (home-page "https://github.com/rust-phf/rust-phf")
   (synopsis "Runtime support for perfect hash function data structures")
   (description "This package provides runtime support for perfect hash
function data structures.")
   (license license:expat)))

(define-public rust-num-cpus-1.16
  (package
   (name "rust-num-cpus")
   (version "1.16.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "num-cpus" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0hra6ihpnh06dvfvz9ipscys0xfqa9ca9hzp384d5m02ssvgqqa1"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-hermit-abi" ,rust-hermit-abi-0.2)
       ("rust-libc" ,rust-libc-0.2))))
   (home-page "https://github.com/seanmonstar/num_cpus")
   (synopsis "Get the number of CPUs on a machine")
   (description
    "Get the number of CPUs on a machine.")
   (license (list license:asl2.0
                  license:expat))))

(define-public rust-libssh2-sys-0.3
  (package
    (name "rust-libssh2-sys")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libssh2-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1vkidqw5ll71ynqc93hgcq62iqkklzb5268zffd13ql7nwqa1j1d"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "libssh2") #t))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ;; Build dependencies:
        ("rust-cc" ,rust-cc-1)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libssh2 openssl zlib))
    (home-page "https://github.com/alexcrichton/ssh2-rs")
    (synopsis "Native bindings to the libssh2 library")
    (description
     "This package provides native rust bindings to the @code{libssh2} library.")
    (license (list license:asl2.0
                   license:expat))))


(define-public rust-libgit2-sys-0.16
  (package
   (name "rust-libgit2-sys")
   (version "0.16.1+1.7.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "libgit2-sys" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "030dnq7hz79qs4rxdllc3ailvqzc432jwwxk7g8av55hh0vbp8pj"))
     (modules '((guix build utils)))
     (snippet
      '(begin (delete-file-recursively "libgit2")))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-cc" ,rust-cc-1)
       ("rust-libc" ,rust-libc-0.2)
       ("rust-libssh2-sys" ,rust-libssh2-sys-0.3)
       ("rust-libz-sys" ,rust-libz-sys-1)
       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
       ("rust-pkg-config" ,rust-pkg-config-0.3))))
   (native-inputs
    (list pkg-config))
   (inputs
    (list libgit2 openssl zlib))
   (home-page "https://github.com/rust-lang/git2-rs")
   (synopsis "Native bindings to the libgit2 library")
   (description
    "This package provides native Rust bindings to the @code{libgit2}
library.")
   (license (list license:expat license:asl2.0))))


(define-public rust-git2-0.18
  (package
   (name "rust-git2")
   (version "0.18.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "git2" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1kf0kvg3i7p1223zs2h9fz99ndm0l9kdx3hcw63g73dh5nlppygv"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags-2.4.0)
       ("rust-libc" ,rust-libc-0.2)
       ("rust-libgit2-sys" ,rust-libgit2-sys-0.16)
       ("rust-log" ,rust-log-0.4)
       ("rust-openssl-probe" ,rust-openssl-probe-0.1)
       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
       ("rust-url" ,rust-url-2))
      #:cargo-development-inputs
      (("rust-paste" ,rust-paste-1)
       ("rust-structopt" ,rust-structopt-0.3)
       ("rust-tempfile" ,rust-tempfile-3)
       ("rust-time" ,rust-time-0.1))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("git" ,git-minimal)))		;for a single test
   (inputs
    (list libgit2 libssh2 openssl zlib))
   (home-page "https://github.com/rust-lang/git2-rs")
   (synopsis "Rust bindings to libgit2")
   (description
    "This package provides bindings to libgit2 for interoperating with git
repositories.  This library is both threadsafe and memory safe and allows both
reading and writing git repositories.")
   (license (list license:expat license:asl2.0))))

(define-public rust-windows-bindgen-0.51
  (package
   (name "rust-windows-bindgen")
   (version "0.51.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "windows-bindgen" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0xfdq4q958qal5iks8xkaanf7w3akzfxc58dxvz7amhjg2vic7xw"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                      ("rust-rayon" ,rust-rayon-1)
                      ("rust-syn" ,rust-syn-2)
                      ("rust-windows-metadata" ,rust-windows-metadata-0.51))))
   (home-page "https://github.com/microsoft/windows-rs")
   (synopsis "Windows metadata compiler")
   (description "Windows metadata compiler")
   (license (list license:expat license:asl2.0))))

(define-public rust-bitflags-2.4.0
  (package
    (name "rust-bitflags")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dc6xa7flfl59makmhixjcrslwlvdxxwrgxbr8p7bkvz53k2ls5l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                                   ("rust-bytemuck" ,rust-bytemuck-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-zerocopy" ,rust-zerocopy-0.6))))
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "A macro to generate structures which behave like bitflags.
")
    (description
     "This package provides a macro to generate structures which behave like bitflags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pure-rust-locales-0.7
  (package
   (name "rust-pure-rust-locales")
   (version "0.7.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "pure-rust-locales" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0cl46srhxzj0jlvfp73l8l9qw54qwa04zywaxdf73hidwqlsh0pd"))))
   (build-system cargo-build-system)
   (home-page "https://github.com/cecton/pure-rust-locales")
   (synopsis
    "Pure Rust locales imported directly from the GNU C Library. `LC_COLLATE` and `LC_CTYPE` are not yet supported.")
   (description
    "Pure Rust locales imported directly from the GNU C Library. `LC_COLLATE` and
`LC_CTYPE` are not yet supported.")
   (license (list license:expat license:asl2.0))))

(define-public rust-android-tzdata-0.1
  (package
   (name "rust-android-tzdata")
   (version "0.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "android-tzdata" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-development-inputs (("rust-zip" ,rust-zip-0.6))))
   (home-page "https://github.com/RumovZ/android-tzdata")
   (synopsis "Parser for the Android-specific tzdata file")
   (description "Parser for the Android-specific tzdata file")
   (license (list license:expat license:asl2.0))))

(define-public rust-chrono-0.4
  (package
   (name "rust-chrono")
   (version "0.4.31")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "chrono" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0f6vg67pipm8cziad2yms6a639pssnvysk1m05dd9crymmdnhb3z"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-android-tzdata" ,rust-android-tzdata-0.1)
                      ("rust-arbitrary" ,rust-arbitrary-1)
                      ("rust-iana-time-zone" ,rust-iana-time-zone-0.1)
                      ("rust-js-sys" ,rust-js-sys-0.3)
                      ("rust-num-traits" ,rust-num-traits-0.2)
                      ("rust-pure-rust-locales" ,rust-pure-rust-locales-0.7)
                      ("rust-rkyv" ,rust-rkyv-0.7)
                      ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
                      ("rust-serde" ,rust-serde-1)
                      ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                      ("rust-windows-targets" ,rust-windows-targets-0.48))
      #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                  ("rust-serde-derive" ,rust-serde-derive-1)
                                  ("rust-serde-json" ,rust-serde-json-1)
                                  ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                  ("rust-windows-bindgen" ,rust-windows-bindgen-0.51))))
   (home-page "https://github.com/chronotope/chrono")
   (synopsis "Date and time library for Rust")
   (description "Date and time library for Rust")
   (license (list license:expat license:asl2.0))))

(define-public rust-ansiterm-0.12
  (package
   (name "rust-ansiterm")
   (version "0.12.2")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "ansiterm" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1k14pywvgd829swxzji0nchk4n6yvr9xz6lkwv96v8ax77sqgdaa"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-ansi-colours" ,rust-ansi-colours-1)
                      ("rust-serde" ,rust-serde-1)
                      ("rust-winapi" ,rust-winapi-0.3))
      #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                  ("rust-regex" ,rust-regex-1)
                                  ("rust-serde-json" ,rust-serde-json-1))))
   (home-page "https://github.com/rustadopt/ansiterm-rs")
   (synopsis "Library for ANSI terminal colours and styles (bold, underline)")
   (description
    "Library for ANSI terminal colours and styles (bold, underline)")
   (license license:expat)))

(define-public rust-linux-raw-sys-0.4
  (package
    (name "rust-linux-raw-sys")
    (version "0.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "linux-raw-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nw8dqdhai0c7r701bicj3y6vrwc7dgbx9lbcw29ijnr7d562liq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))
       #:cargo-development-inputs (("rust-libc" ,rust-libc-0.2)
                                   ("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/sunfishcode/linux-raw-sys")
    (synopsis "Generated bindings for Linux's userspace API")
    (description "Generated bindings for Linux's userspace API")
    (license #f)))

(define-public rust-libc-0.2.148
  (package
    (name "rust-libc")
    (version "0.2.148")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16rn9l8s5sj9n2jb2pw13ghqwa5nvjggkh9q3lp6vs1jfghp3p4w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-memoffset-0.9
  (package
    (name "rust-memoffset")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memoffset" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v20ihhdzkfw1jx00a7zjpk2dcp5qjq6lz302nyqamd9c4f4nqss"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://github.com/Gilnaa/memoffset")
    (synopsis "offset_of functionality for Rust structs.")
    (description "offset_of functionality for Rust structs.")
    (license license:expat)))

(define-public rust-lock-api-0.4.10
  (package
    (name "rust-lock-api")
    (version "0.4.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lock-api" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05nd9nzxqidg24d1k8y5vlc8lz9gscpskrikycib46qbl8brgk61"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-owning-ref" ,rust-owning-ref-0.4)
                       ("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
     "Wrappers to create fully-featured Mutex and @code{RwLock} types.  Compatible
with no_std.")
    (license (list license:expat license:asl2.0))))

(define-public rust-parking-lot-core-0.9.8
  (package
    (name "rust-parking-lot-core")
    (version "0.9.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "parking_lot_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ixlak319bpzldq20yvyfqk0y1vi736zxbw101jvzjp7by30rw4k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-petgraph" ,rust-petgraph-0.6)
                       ("rust-redox-syscall" ,rust-redox-syscall-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thread-id" ,rust-thread-id-4)
                       ("rust-windows-targets" ,rust-windows-targets-0.48))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "An advanced API for creating custom synchronization primitives.")
    (description
     "An advanced API for creating custom synchronization primitives.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dashmap-5
  (package
   (name "rust-dashmap")
   (version "5.5.3")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "dashmap" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                      ("rust-cfg-if" ,rust-cfg-if-1)
                      ("rust-hashbrown" ,rust-hashbrown-0.14)
                      ("rust-lock-api" ,rust-lock-api-0.4.10)
                      ("rust-once-cell" ,rust-once-cell-1)
                      ("rust-parking-lot-core" ,rust-parking-lot-core-0.9.8)
                      ("rust-rayon" ,rust-rayon-1)
                      ("rust-serde" ,rust-serde-1))))
   (home-page "https://github.com/xacrimon/dashmap")
   (synopsis "Blazing fast concurrent HashMap for Rust.")
   (description "Blazing fast concurrent @code{HashMap} for Rust.")
   (license license:expat)))

(define-public rust-serial-test-derive-2
  (package
    (name "rust-serial-test-derive")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serial-test-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13zvd5ds76hhjn3z0axc05n15lzpxpz77jcykic8q5knhlbjklci"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.10))))
    (home-page "https://github.com/palfrey/serial_test/")
    (synopsis "Helper crate for serial_test")
    (description "Helper crate for serial_test")
    (license license:expat)))

(define-public rust-serial-test-2
  (package
   (name "rust-serial-test")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "serial_test" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0b9v0csv9wxl1gcjq99plwimxbmhgr6kzbwqyb457qh3d22xsmhf"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                      ("rust-document-features" ,rust-document-features-0.2)
                      ("rust-fslock" ,rust-fslock-0.2)
                      ("rust-futures" ,rust-futures-0.3)
                      ("rust-lazy-static" ,rust-lazy-static-1)
                      ("rust-log" ,rust-log-0.4)
                      ("rust-parking-lot" ,rust-parking-lot-0.12)
                      ("rust-serial-test-derive" ,rust-serial-test-derive-2))
      #:cargo-development-inputs (("rust-itertools" ,rust-itertools-0.10)
                                  ("rust-tokio" ,rust-tokio-1))))
   (home-page "https://github.com/palfrey/serial_test/")
   (synopsis "Allows for the creation of serialised Rust tests")
   (description "Allows for the creation of serialised Rust tests")
   (license license:expat)))

(define-public rust-rustix-0.38
  (package
   (name "rust-rustix")
   (version "0.38.18")
   (source (origin
            (method url-fetch)
            (uri (crate-uri "rustix" version))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "175igcis7whnpcg3z9ykmayqkf49jdypsi22kc9wv095ghnywx2s"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-bitflags" ,rust-bitflags-2.4.0)
       ("rust-cc" ,rust-cc-1)
       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
       ("rust-errno" ,rust-errno-0.3)
       ("rust-io-lifetimes" ,rust-io-lifetimes-1)
       ("rust-itoa" ,rust-itoa-1)
       ("rust-libc" ,rust-libc-0.2.148)
       ("rust-linux-raw-sys" ,rust-linux-raw-sys-0.4)
       ("rust-once-cell" ,rust-once-cell-1)
       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
       ("rust-windows-sys" ,rust-windows-sys-0.48))
      #:cargo-development-inputs
      (("rust-criterion" ,rust-criterion-0.4)
       ("rust-ctor" ,rust-ctor-0.2)
       ("rust-errno" ,rust-errno-0.3)
       ("rust-flate2" ,rust-flate2-1)
       ("rust-io-lifetimes" ,rust-io-lifetimes-1)
       ("rust-libc" ,rust-libc-0.2)
       ("rust-memoffset" ,rust-memoffset-0.9)
       ("rust-serial-test" ,rust-serial-test-2)
       ("rust-tempfile" ,rust-tempfile-3))))
   (home-page "https://github.com/bytecodealliance/rustix")
   (synopsis "Safe Rust bindings to POSIX syscalls")
   (description
    "This package provides safe Rust bindings to POSIX syscalls.")
   ;; Apache 2.0, Apache 2.0 with LLVM exception, or Expat.
   (license (list license:asl2.0 license:expat))))

(define-public rust-terminal-size-0.3
  (package
   (name "rust-terminal-size")
   (version "0.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "terminal-size" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1xqdzdjq77smg41z67vg3qwrcilf1zf5330gdrgm22lyghmvzgi1"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-rustix" ,rust-rustix-0.38)
                      ("rust-windows-sys" ,rust-windows-sys-0.48))))
   (home-page "https://github.com/eminence/terminal-size")
   (synopsis "Gets the size of your Linux or Windows terminal")
   (description "Gets the size of your Linux or Windows terminal")
   (license (list license:expat license:asl2.0))))

(define-public rust-terminal-size-0.2.6
  (package
    (name "rust-terminal-size")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "terminal-size" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0drj7gb77kay5r1cv53ysq3g9g4f8n0jkhld0kadi3lzkvqzcswf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rustix" ,rust-rustix-0.37)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/eminence/terminal-size")
    (synopsis "Gets the size of your Linux or Windows terminal")
    (description "Gets the size of your Linux or Windows terminal")
    (license (list license:expat license:asl2.0))))


(define-public rust-isolang-2
  (package
   (name "rust-isolang")
   (version "2.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "isolang" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0wg07gh7a1cvx0c1w238d7l4vh17cwwvjmw729qqnw5wn4fj43zq"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-phf" ,rust-phf-0.11)
                      ("rust-serde" ,rust-serde-1))
      #:cargo-development-inputs (("rust-phf-codegen" ,rust-phf-codegen-0.11)
                                  ("rust-serde-json" ,rust-serde-json-1))))
   (home-page "https://github.com/humenda/isolang-rs")
   (synopsis "Efficient, static lookup table for ISO 639 language codes")
   (description "Efficient, static lookup table for ISO 639 language codes")
   (license license:asl2.0)))

(define-public rust-timeago-0.4
  (package
   (name "rust-timeago")
   (version "0.4.2")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "timeago" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1rnh92sh1l4jbjvz4g7xvcvmfh7nk5k7mm2w56pnm9z0kmc0wwd1"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                      ("rust-isolang" ,rust-isolang-2))))
   (home-page "https://github.com/vi/timeago")
   (synopsis
    "Given a Duration, lossily format it like in 'N days ago'. Parsing it back to Duration is not supported yet.")
   (description
    "Given a Duration, lossily format it like in N days ago'.  Parsing it back to
Duration is not supported yet.")
   (license (list license:expat license:asl2.0))))

(define-public rust-randomize-3
  (package
    (name "rust-randomize")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "randomize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02ll7r3rrpmhjx34w91m1yvqw1685bq2n9amqvycjcqznncqrhw8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rand-core" ,rust-rand-core-0.5)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1))))
    (home-page "https://github.com/Lokathor/randomize")
    (synopsis "Randomization routines")
    (description "Randomization routines")
    (license license:bsd-0)))

(define-public rust-oorandom-11
  (package
   (name "rust-oorandom")
   (version "11.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "oorandom" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0xdm4vd89aiwnrk1xjwzklnchjqvib4klcihlc2bsd4x50mbrc8a"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-development-inputs (("rust-rand-core" ,rust-rand-core-0.5)
                                  ("rust-rand-pcg" ,rust-rand-pcg-0.2)
                                  ("rust-random-fast-rng" ,rust-random-fast-rng-0.1)
                                  ("rust-randomize" ,rust-randomize-3))))
   (home-page "https://sr.ht/~icefox/oorandom/")
   (synopsis "A tiny, robust PRNG implementation.")
   (description "This package provides a tiny, robust PRNG implementation.")
   (license license:expat)))

(define-public rust-criterion-0.5
  (package
   (name "rust-criterion")
   (version "0.5.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "criterion" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0bv9ipygam3z8kk6k771gh9zi0j0lb9ir0xi1pc075ljg80jvcgj"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-anes" ,rust-anes-0.1)
                      ("rust-async-std" ,rust-async-std-1)
                      ("rust-cast" ,rust-cast-0.3)
                      ("rust-ciborium" ,rust-ciborium-0.2)
                      ("rust-clap" ,rust-clap-4)
                      ("rust-criterion-plot" ,rust-criterion-plot-0.5)
                      ("rust-csv" ,rust-csv-1)
                      ("rust-futures" ,rust-futures-0.3)
                      ("rust-is-terminal" ,rust-is-terminal-0.4)
                      ("rust-itertools" ,rust-itertools-0.10)
                      ("rust-num-traits" ,rust-num-traits-0.2)
                      ("rust-once-cell" ,rust-once-cell-1)
                      ("rust-oorandom" ,rust-oorandom-11)
                      ("rust-plotters" ,rust-plotters-0.3)
                      ("rust-rayon" ,rust-rayon-1)
                      ("rust-regex" ,rust-regex-1)
                      ("rust-serde" ,rust-serde-1)
                      ("rust-serde-derive" ,rust-serde-derive-1)
                      ("rust-serde-json" ,rust-serde-json-1)
                      ("rust-smol" ,rust-smol-1)
                      ("rust-tinytemplate" ,rust-tinytemplate-1)
                      ("rust-tokio" ,rust-tokio-1)
                      ("rust-walkdir" ,rust-walkdir-2))
      #:cargo-development-inputs (("rust-approx" ,rust-approx-0.5)
                                  ("rust-futures" ,rust-futures-0.3)
                                  ("rust-quickcheck" ,rust-quickcheck-1)
                                  ("rust-rand" ,rust-rand-0.8)
                                  ("rust-tempfile" ,rust-tempfile-3))))
   (home-page "https://bheisler.github.io/criterion.rs/book/index.html")
   (synopsis "Statistics-driven micro-benchmarking library")
   (description "Statistics-driven micro-benchmarking library")
   (license (list license:asl2.0 license:expat))))

(define-public rust-partition-identity-0.3
  (package
    (name "rust-partition-identity")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "partition-identity" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08cymccnyf2b0pwc7x4wj889k1knqmrb8500idsjslybpvwjbacz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/pop-os/partition-identity")
    (synopsis
     "Find the ID of a device by its path, or find a device path by its ID.")
    (description
     "Find the ID of a device by its path, or find a device path by its ID.")
    (license license:expat)))

(define-public rust-proc-mounts-0.3
  (package
   (name "rust-proc-mounts")
   (version "0.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "proc-mounts" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1wpw3z2qq8wm3da8d0253a1h95nma6lad41m9yzp1ayh6n22yr8d"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-partition-identity" ,rust-partition-identity-0.3))))
   (home-page "https://github.com/pop-os/proc-mounts")
   (synopsis "Fetch active mounts and swaps on a Linux system")
   (description "Fetch active mounts and swaps on a Linux system")
   (license license:expat)))

(define-public rust-uzers-0.11
  (package
    (name "rust-uzers")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uzers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qrzbhncbv4s52lgyzs2pxn1b6gmx9k7h1rdwdwix44cgvf87lkn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.7))))
    (home-page "https://github.com/rustadopt/uzers-rs")
    (synopsis
     "Continuation of users, a library for accessing Unix users and groups")
    (description
     "Continuation of users, a library for accessing Unix users and groups")
    (license license:expat)))

;; needs rust 1.30 which isn't currently in guix [2023-10-18 Wed] (no unstable builds either to push the feature option in)
;; (define-public eza-0.14.1
;;   (package
;;    (name "eza")
;;    (version "0.14.1")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri (crate-uri "eza" version))
;;      (file-name
;;       (string-append name "-" version ".tar.gz"))
;;      (sha256
;;       (base32
;;        "0vqni5kv4vsi8j1k6gvylp1jq9wcv4m4a6bc4vjrb8sx0dx5l37c"))
;;      (modules '((guix build utils)))
;;               (snippet
;;                '(begin
;; 		  (substitute* "Cargo.toml"
;; 			       (("rust-version = \"1\\.70\\.0\"")
;; 				"rust-version = \"1.68.0\""))
;; 		  (substitute* "src/main.rs"
;; 			       (("#!\\[allow\\(clippy::wildcard_imports\\)\\]")
;; 				"#![allow(clippy::wildcard_imports)]\n#![feature(one_cell)]\n#![feature(is_some_and)]")))))) ;;wants 1.7
;;    (build-system cargo-build-system)
;;    (arguments
;;     `(
;;       #:install-source? #f
;;       #:cargo-inputs
;;       (("rust-ansiterm" ,rust-ansiterm-0.12)
;;        ("rust-chrono" ,rust-chrono-0.4) 
;;        ("rust-git2" ,rust-git2-0.18)	   
;;        ("rust-glob" ,rust-glob-0.3)
;;        ("rust-lazy-static" ,rust-lazy-static-1)
;;        ("rust-libc" ,rust-libc-0.2)
;;        ("rust-locale" ,rust-locale-0.2)
;;        ("rust-log" ,rust-log-0.4)
;;        ("rust-natord" ,rust-natord-1)
;;        ("rust-num-cpus" ,rust-num-cpus-1.16)
;;        ("rust-number-prefix" ,rust-number-prefix-0.4)
;;        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
;;        ("rust-term-grid" ,rust-term-grid-0.1)
;;        ("rust-terminal-size" ,rust-terminal-size-0.3)
;;        ("rust-unicode-width" ,rust-unicode-width-0.1)
;;        ("rust-users" ,rust-users-0.11) ;;uzers 11.3?
;;        ("rust-zoneinfo-compiled" ,rust-zoneinfo-compiled-0.5)
;;        ("rust-phf" ,rust-phf-0.11.2)
;;        ("rust-timeago" ,rust-timeago-0.4)
;;        ("rust-proc-mounts" ,rust-proc-mounts-0.3)
;;        ("rust-uzers" ,rust-uzers-0.11))
;;       #:cargo-development-inputs
;;       (("rust-criterion" ,rust-criterion-0.5)
;;        ("rust-trycmd" ,rust-trycmd-0.14))))
;;    (inputs (list libgit2 zlib))
;;    (native-inputs
;;     (append
;;      (list pkg-config)
;;      (if (member (%current-system)
;;                  (package-transitive-supported-systems pandoc))
;;          (list pandoc)
;;          '())))
;;    (home-page "")
;;    (synopsis "")
;;    (description "")
;;    (license #f)))

(define-public rust-gethostname-0.4.3
  (package
   (name "rust-gethostname")
   (version "0.4.3")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "gethostname" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "063qqhznyckwx9n4z4xrmdv10s0fi6kbr17r6bi1yjifki2y0xh1"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                      ("rust-windows-targets" ,rust-windows-targets-0.48))))
   (home-page "https://github.com/swsnr/gethostname.rs")
   (synopsis "gethostname for all platforms")
   (description "gethostname for all platforms")
   (license license:asl2.0)))

(define-public rust-urlencoding-2.1.3
  (package
    (name "rust-urlencoding")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urlencoding" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))))
    (build-system cargo-build-system)
    (home-page "https://lib.rs/urlencoding")
    (synopsis "A Rust library for doing URL percentage encoding.")
    (description
     "This package provides a Rust library for doing URL percentage encoding.")
    (license license:expat)))

(define-public eza-0.12.0
  (package
   (name "eza")
   (version "0.12.0")
   (source (origin
	    (method url-fetch)
	    (uri (crate-uri "eza" version))
	    (file-name
	     (string-append name "-" version ".tar.gz"))
	    (sha256
	     (base32
	      "0akx5j4cln2a8paphnism7qy1qdghkpx0vpdc3h0k93cxlyrlzqx"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs (("rust-ansiterm" ,rust-ansiterm-0.12)
                      ("rust-chrono" ,rust-chrono-0.4)
                      ("rust-gethostname" ,rust-gethostname-0.4.3)
                      ("rust-git2" ,rust-git2-0.18)
                      ("rust-glob" ,rust-glob-0.3)
                      ("rust-lazy-static" ,rust-lazy-static-1)
                      ("rust-libc" ,rust-libc-0.2)
                      ("rust-locale" ,rust-locale-0.2)
                      ("rust-log" ,rust-log-0.4)
                      ("rust-natord" ,rust-natord-1)
                      ("rust-num-cpus" ,rust-num-cpus-1.16)
                      ("rust-number-prefix" ,rust-number-prefix-0.4)
                      ("rust-phf" ,rust-phf-0.11.2)
                      ("rust-proc-mounts" ,rust-proc-mounts-0.3)
                      ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
                      ("rust-term-grid" ,rust-term-grid-0.1)
                      ("rust-terminal-size" ,rust-terminal-size-0.2.6)
                      ("rust-timeago" ,rust-timeago-0.4)
                      ("rust-unicode-width" ,rust-unicode-width-0.1)
                      ("rust-urlencoding" ,rust-urlencoding-2.1.3)
                      ("rust-uzers" ,rust-uzers-0.11)
                      ("rust-zoneinfo-compiled" ,rust-zoneinfo-compiled-0.5))
      #:cargo-development-inputs (("rust-trycmd" ,rust-trycmd-0.14))))
   (native-inputs (list pkg-config))
   (inputs (list zlib libgit2 libssh2 openssl)) ;;manually added becuase a dependency uses it, but can't be found during build
   (home-page "https://github.com/eza-community/eza")
   (synopsis "A modern replacement for ls")
   (description "This package provides a modern replacement for ls")
   (license license:expat) ))
