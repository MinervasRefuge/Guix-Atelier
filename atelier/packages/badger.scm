(define-module (atelier packages badger)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages jemalloc)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module ((guix licenses) #:prefix license:))

(define unknown-license! #f)

(define-public go-github-com-cespare-xxhash-v2-2.2.0
  (package
   (name "go-github-com-cespare-xxhash-v2")
   (version "2.2.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/cespare/xxhash")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "055xvgyv78xl6bdj8kgy0105n9cq33w4rb0sg84lp9r85i9qx2l5"))))
   (build-system go-build-system)
   (arguments
    '(#:import-path "github.com/cespare/xxhash/v2"
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
                     ;; Source-only package
                     (delete 'build))))
   (home-page "https://github.com/cespare/xxhash")
   (synopsis "xxhash")
   (description
    "Package xxhash implements the 64-bit variant of xxHash (XXH64) as described at
@@url{http://cyan4973.github.io/xxHash/,http://cyan4973.github.io/xxHash/}.")
   (license license:expat)))

;; todo investigate
;; // Compile jemalloc with ./configure --with-jemalloc-prefix="je_"
;; // https://android.googlesource.com/platform/external/jemalloc_new/+/6840b22e8e11cb68b493297a5cd757d6eaa0b406/TUNING.md
;; // These two config options seems useful for frequent allocations and deallocations in
;; // multi-threaded programs (like we have).
;; // JE_MALLOC_CONF="background_thread:true,metadata_thp:auto"
;; //
;; // Compile Go program with `go build -tags=jemalloc` to enable this.

(define jemalloc-alt
  (package
    (inherit jemalloc)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'delete-thp-test
           ;; This test does not check if transparent huge pages are supported
           ;; on the system before running the test.
           (lambda _
             (substitute* "Makefile.in"
               (("\\$\\(srcroot\\)test/unit/pages.c \\\\") "\\")))))
      #:configure-flags
      ''("--disable-initial-exec-tls"
         "--with-jemalloc-prefix=je_")))))

(define-public go-github-com-dgraph-io-ristretto-0.1.1
  (package
    (name "go-github-com-dgraph-io-ristretto")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dgraph-io/ristretto")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mjni3zaxvjvw5c7nh4sij13sslg92x9xi3ykxzbv2s6g2ynigss"))))
    (build-system go-build-system)
    ;; (inputs (list jemalloc-alt))
    (arguments
     '(#:import-path "github.com/dgraph-io/ristretto"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-compile-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/github.com/dgraph-io/ristretto/z/calloc_jemalloc.go"
               (("#cgo LDFLAGS: /usr/local/lib/libjemalloc.a -L/usr/local/lib -Wl,-rpath,/usr/local/lib -ljemalloc -lm -lstdc\\+\\+ -pthread -ldl")
                "#cgo LDFLAGS: -ljemalloc -lm -lstdc++ -pthread -ldl"
                ;; (string-append "#cgo LDFLAGS: "
                ;;                (search-input-file inputs "lib/libjemalloc.a")
                ;;                " -ljemalloc -lm -lstdc++ -pthread -ldl")
                ))))
         ;; Source-only package
         (delete 'build))))
    ;; (propagated-inputs `(("go-golang-org-x-sys-0.0.0-20221010170243-090e33056c14" ,go-golang-org-x-sys-0.0.0-20221010170243-090e33056c14)
    ;;                      ("go-github-com-stretchr-testify-1.4.0" ,go-github-com-stretchr-testify-1.4.0)
    ;;                      ("go-github-com-pkg-errors-0.9.1" ,go-github-com-pkg-errors-0.9.1)
    ;;                      ("go-github-com-golang-glog-0.0.0-20160126235308-23def4e6c14b" ,go-github-com-golang-glog-0.0.0-20160126235308-23def4e6c14b)
    ;;                      ("go-github-com-dustin-go-humanize-1.0.0" ,go-github-com-dustin-go-humanize-1.0.0)
    ;;                      ("go-github-com-dgryski-go-farm-0.0.0-20190423205320-6a90982ecee2" ,go-github-com-dgryski-go-farm-0.0.0-20190423205320-6a90982ecee2)
    ;;                      ("go-github-com-davecgh-go-spew-1.1.1" ,go-github-com-davecgh-go-spew-1.1.1)
    ;;                      ("go-github-com-cespare-xxhash-v2-2.1.1" ,go-github-com-cespare-xxhash-v2-2.1.1)))
    (home-page "https://github.com/dgraph-io/ristretto")
    (synopsis "Ristretto")
    (description
     "Ristretto is a fast, fixed size, in-memory cache with a dual focus on throughput
and hit ratio performance.  You can easily add Ristretto to an existing system
and keep the most valuable data where you need it.")
    (license license:asl2.0)))

(define-public go-github-com-dustin-go-humanize-1.0.0
  (package
    (name "go-github-com-dustin-go-humanize")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dustin/go-humanize")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kqf1kavdyvjk7f8kx62pnm7fbypn9z1vbf8v2qdh3y7z7a0cbl3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/dustin/go-humanize"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://github.com/dustin/go-humanize")
    (synopsis "Humane Units")
    (description
     "Package humanize converts boring ugly numbers to human-friendly strings and
back.")
    (license license:expat)))

(define-public go-github-com-gogo-protobuf-1.3.2
  (package
    (name "go-github-com-gogo-protobuf")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gogo/protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dfv1bhx5zhb5bsj5sj757nkacf2swp1ajpcyj9d0b37n602m18a"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gogo/protobuf"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-golang-org-x-tools-0.0.0-20210106214847-113979e3529a" ,go-golang-org-x-tools-0.0.0-20210106214847-113979e3529a)
    ;;                      ("go-github-com-kisielk-gotool-1.0.0" ,go-github-com-kisielk-gotool-1.0.0)
    ;;                      ("go-github-com-kisielk-errcheck-1.5.0" ,go-github-com-kisielk-errcheck-1.5.0)))
    (home-page "https://github.com/gogo/protobuf")
    (synopsis "Protocol Buffers for Go with Gadgets")
    (description
     "gogoprotobuf is a fork of
@@url{https://github.com/golang/protobuf,golang/protobuf} with extra code
generation features.")
    (license license:bsd-3)))

(define-public go-github-com-golang-protobuf-1.5.2
  (package
    (name "go-github-com-golang-protobuf")
    (version "1.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mh5fyim42dn821nsd3afnmgscrzzhn3h8rag635d2jnr23r1zhk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golang/protobuf"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-google-golang-org-protobuf-1.26.0" ,go-google-golang-org-protobuf-1.26.0)
    ;;                      ("go-github-com-google-go-cmp-0.5.5" ,go-github-com-google-go-cmp-0.5.5)))
    (home-page "https://github.com/golang/protobuf")
    (synopsis "Go support for Protocol Buffers")
    (description
     "This module (@@url{https://pkg.go.dev/mod/github.com/golang/protobuf,(code
github.com/golang/protobuf)}) contains Go bindings for protocol buffers.")
    (license license:bsd-3)))

(define-public go-github-com-golang-snappy-0.0.3
  (package
    (name "go-github-com-golang-snappy")
    (version "0.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/snappy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dc8sdca0nrqb8wri91mi2xcjm16wyawm4y0fwc5gp24ahjbrg7g"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golang/snappy"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://github.com/golang/snappy")
    (synopsis #f)
    (description
     "Package snappy implements the Snappy compression format.  It aims for very high
speeds and reasonable compression.")
    (license license:bsd-3)))

(define-public go-github-com-google-flatbuffers-1.12.1
  (package
    (name "go-github-com-google-flatbuffers")
    (version "1.12.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/flatbuffers")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qwry9c275rvgbgvmqqinp2pjm26xap30kpiwxfzxiwnb9vdvhg6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/flatbuffers"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://github.com/google/flatbuffers")
    (synopsis "FlatBuffers")
    (description
     "@@strong{FlatBuffers} is a cross platform serialization library architected for
maximum memory efficiency.  It allows you to directly access serialized data
without parsing/unpacking it first, while still having great forwards/backwards
compatibility.")
    (license license:asl2.0)))

(define-public go-github-com-klauspost-compress-1.12.3
  (package
    (name "go-github-com-klauspost-compress")
    (version "1.12.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/klauspost/compress")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rmqbkxgpwsnvqicpjn0ya7saq4djnpnrwy1wkk6f0nvwhdgn268"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/klauspost/compress"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-github-com-golang-snappy-0.0.3" ,go-github-com-golang-snappy-0.0.3)))
    (home-page "https://github.com/klauspost/compress")
    (synopsis "compress")
    (description "This package provides various compression algorithms.")
    (license unknown-license!)))

(define-public go-github-com-pkg-errors-0.9.1
  (package
    (name "go-github-com-pkg-errors")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pkg/errors")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1761pybhc2kqr6v5fm8faj08x9bql8427yqg6vnfv6nhrasx1mwq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/pkg/errors"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://github.com/pkg/errors")
    (synopsis "errors")
    (description "Package errors provides simple error handling primitives.")
    (license license:bsd-2)))

(define-public go-github-com-spf13-cobra-0.0.5
  (package
    (name "go-github-com-spf13-cobra")
    (version "0.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/spf13/cobra")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z4x8js65mhwg1gf6sa865pdxfgn45c3av9xlcc1l3xjvcnx32v2"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/spf13/cobra"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-gopkg-in-yaml-v2-2.2.2" ,go-gopkg-in-yaml-v2-2.2.2)
    ;;                      ("go-github-com-spf13-viper-1.3.2" ,go-github-com-spf13-viper-1.3.2)
    ;;                      ("go-github-com-spf13-pflag-1.0.3" ,go-github-com-spf13-pflag-1.0.3)
    ;;                      ("go-github-com-mitchellh-go-homedir-1.1.0" ,go-github-com-mitchellh-go-homedir-1.1.0)
    ;;                      ("go-github-com-inconshreveable-mousetrap-1.0.0" ,go-github-com-inconshreveable-mousetrap-1.0.0)
    ;;                      ("go-github-com-cpuguy83-go-md2man-1.0.10" ,go-github-com-cpuguy83-go-md2man-1.0.10)
    ;;                      ("go-github-com-burntsushi-toml-0.3.1" ,go-github-com-burntsushi-toml-0.3.1)))
    (home-page "https://github.com/spf13/cobra")
    (synopsis "Overview")
    (description
     "Package cobra is a commander providing a simple interface to create powerful
modern CLI interfaces.  In addition to providing an interface, Cobra
simultaneously provides a controller to organize your application code.")
    (license license:asl2.0)))

(define-public go-github-com-stretchr-testify-1.4.0
  (package
    (name "go-github-com-stretchr-testify")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/stretchr/testify")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "187i5g88sxfy4vxpm7dw1gwv29pa2qaq475lxrdh5livh69wqfjb"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/stretchr/testify"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-gopkg-in-yaml-v2-2.2.2" ,go-gopkg-in-yaml-v2-2.2.2)
    ;;                      ("go-github-com-stretchr-objx-0.1.0" ,go-github-com-stretchr-objx-0.1.0)
    ;;                      ("go-github-com-pmezard-go-difflib-1.0.0" ,go-github-com-pmezard-go-difflib-1.0.0)
    ;;                      ("go-github-com-davecgh-go-spew-1.1.0" ,go-github-com-davecgh-go-spew-1.1.0)))
    (home-page "https://github.com/stretchr/testify")
    (synopsis "Testify - Thou Shalt Write Tests")
    (description
     "** We are working on testify v2 and would love to hear what you'd like to see in
it, have your say here: @@url{https://cutt.ly/testify,https://cutt.ly/testify}
** Package testify is a set of packages that provide many tools for testifying
that your code will behave as you intend.")
    (license license:expat)))

(define-public go-go-opencensus-io-0.22.5
  (package
    (name "go-go-opencensus-io")
    (version "0.22.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/census-instrumentation/opencensus-go")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18s1d7bk28i8gbs6i587ccd31qp3f8gibgd14dpkzs05gyvlccy6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.opencensus.io"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-google-golang-org-grpc-1.20.1" ,go-google-golang-org-grpc-1.20.1)
    ;;                      ("go-google-golang-org-genproto-0.0.0-20190425155659-357c62f0e4bb" ,go-google-golang-org-genproto-0.0.0-20190425155659-357c62f0e4bb)
    ;;                      ("go-golang-org-x-text-0.3.3" ,go-golang-org-x-text-0.3.3)
    ;;                      ("go-golang-org-x-sys-0.0.0-20190502145724-3ef323f4f1fd" ,go-golang-org-x-sys-0.0.0-20190502145724-3ef323f4f1fd)
    ;;                      ("go-golang-org-x-net-0.0.0-20190620200207-3b0461eec859" ,go-golang-org-x-net-0.0.0-20190620200207-3b0461eec859)
    ;;                      ("go-github-com-stretchr-testify-1.4.0" ,go-github-com-stretchr-testify-1.4.0)
    ;;                      ("go-github-com-google-go-cmp-0.3.0" ,go-github-com-google-go-cmp-0.3.0)
    ;;                      ("go-github-com-golang-protobuf-1.3.1" ,go-github-com-golang-protobuf-1.3.1)
    ;;                      ("go-github-com-golang-groupcache-0.0.0-20190702054246-869f871628b6" ,go-github-com-golang-groupcache-0.0.0-20190702054246-869f871628b6)))
    (home-page "https://go.opencensus.io")
    (synopsis "OpenCensus Libraries for Go")
    (description "Package opencensus contains Go support for OpenCensus.")
    (license license:asl2.0)))

(define-public go-golang-org-x-net-0.7.0
  (package
    (name "go-golang-org-x-net")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/net")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ch88x96in2773ynci7ih2w929qd67mzjqc3nazajcid3ijmh1if"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/net"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-golang-org-x-text-0.7.0" ,go-golang-org-x-text-0.7.0)
    ;;                      ("go-golang-org-x-term-0.5.0" ,go-golang-org-x-term-0.5.0)
    ;;                      ("go-golang-org-x-sys-0.5.0" ,go-golang-org-x-sys-0.5.0)))
    (home-page "https://golang.org/x/net")
    (synopsis "Go Networking")
    (description
     "This repository holds supplementary Go networking libraries.")
    (license license:bsd-3)))

(define-public go-golang-org-x-sys-0.5.0
  (package
    (name "go-golang-org-x-sys")
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/sys")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ypmc9bbxxz1wjl8bp2233x7qgfpiz72d0smm0jng1z5bdjmayx7"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/sys"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://golang.org/x/sys")
    (synopsis "sys")
    (description
     "This repository holds supplemental Go packages for low-level interactions with
the operating system.")
    (license license:bsd-3)))

;;; indirect

(define-public go-github-com-davecgh-go-spew-1.1.1
  (package
    (name "go-github-com-davecgh-go-spew")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/davecgh/go-spew")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hka6hmyvp701adzag2g26cxdj47g21x6jz4sc6jjz1mn59d474y"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/davecgh/go-spew"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://github.com/davecgh/go-spew")
    (synopsis "go-spew")
    (description
     "Go-spew implements a deep pretty printer for Go data structures to aid in
debugging.  A comprehensive suite of tests with 100% test coverage is provided
to ensure proper functionality.  See @@code{test_coverage.txt} for the gocov
coverage report.  Go-spew is licensed under the liberal ISC license, so it may
be used in open source or commercial projects.")
    (license license:isc)))

(define-public go-github-com-golang-glog-1.0.0
  (package
    (name "go-github-com-golang-glog")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/glog")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vm206qrvhn3d571bqcman6fnavw4y3a31ffrmv2xkk0li74h2bf"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golang/glog"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://github.com/golang/glog")
    (synopsis "glog")
    (description
     "Package glog implements logging analogous to the Google-internal C++
INFO/ERROR/V setup.  It provides functions Info, Warning, Error, Fatal, plus
formatting variants such as Infof.  It also provides V-style logging controlled
by the -v and -vmodule=file=2 flags.")
    (license license:asl2.0)))

(define-public go-github-com-golang-groupcache-0.0.0-20190702054246-869f871628b6
  (package
    (name "go-github-com-golang-groupcache")
    (version "0.0.0-20190702054246-869f871628b6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/groupcache")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r4nk8129bvx50qb4xzjaay39b2h6k7cbdqqzdlanmc82ygczsbw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golang/groupcache"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://github.com/golang/groupcache")
    (synopsis "groupcache")
    (description
     "Package groupcache provides a data loading mechanism with caching and
de-duplication that works across a set of peer processes.")
    (license license:asl2.0)))

(define-public go-github-com-inconshreveable-mousetrap-1.0.0
  (package
    (name "go-github-com-inconshreveable-mousetrap")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/inconshreveable/mousetrap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mn0kg48xkd74brf48qf5hzp0bc6g8cf5a77w895rl3qnlpfw152"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/inconshreveable/mousetrap"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://github.com/inconshreveable/mousetrap")
    (synopsis "mousetrap")
    (description "mousetrap is a tiny library that answers a single question.")
    (license license:asl2.0)))

(define-public go-github-com-kr-pretty-0.1.0
  (package
    (name "go-github-com-kr-pretty")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kr/pretty")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18m4pwg2abd0j9cn5v3k2ksk9ig4vlwxmlw9rrglanziv9l967qp"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kr/pretty"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-github-com-kr-text-0.1.0" ,go-github-com-kr-text-0.1.0)))
    (home-page "https://github.com/kr/pretty")
    (synopsis #f)
    (description
     "Package pretty provides pretty-printing for Go values.  This is useful during
debugging, to avoid wrapping long output lines in the terminal.")
    (license license:expat)))

(define-public go-github-com-pmezard-go-difflib-1.0.0
  (package
    (name "go-github-com-pmezard-go-difflib")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pmezard/go-difflib")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c1cn55m4rypmscgf0rrb88pn58j3ysvc2d0432dp3c6fqg6cnzw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/pmezard/go-difflib"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://github.com/pmezard/go-difflib")
    (synopsis "go-difflib")
    (description
     "Go-difflib is a partial port of python 3 difflib package.  Its main goal was to
make unified and context diff available in pure Go, mostly for testing purposes.")
    (license license:bsd-3)))

(define-public go-github-com-spf13-pflag-1.0.3
  (package
    (name "go-github-com-spf13-pflag")
    (version "1.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/spf13/pflag")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cj3cjm7d3zk0mf1xdybh0jywkbbw7a6yr3y22x9sis31scprswd"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/spf13/pflag"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://github.com/spf13/pflag")
    (synopsis "Description")
    (description
     "Package pflag is a drop-in replacement for Go's flag package, implementing
POSIX/GNU-style --flags.")
    (license license:bsd-3)))

(define-public go-google-golang-org-genproto-0.0.0-20230110181048-76db0878b65f
  (package
    (name "go-google-golang-org-genproto")
    (version "0.0.0-20230110181048-76db0878b65f")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/googleapis/go-genproto")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kw1qban9xq3qgb0smrycxfxv5i4c3pwzflib7zhgp36w7hjq9gs"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "google.golang.org/genproto"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-golang-org-x-text-0.4.0" ,go-golang-org-x-text-0.4.0)
    ;;                      ("go-golang-org-x-sys-0.0.0-20220728004956-3c1f35247d10" ,go-golang-org-x-sys-0.0.0-20220728004956-3c1f35247d10)
    ;;                      ("go-golang-org-x-net-0.0.0-20221014081412-f15817d10f9b" ,go-golang-org-x-net-0.0.0-20221014081412-f15817d10f9b)
    ;;                      ("go-google-golang-org-protobuf-1.28.1" ,go-google-golang-org-protobuf-1.28.1)
    ;;                      ("go-google-golang-org-grpc-1.51.0" ,go-google-golang-org-grpc-1.51.0)
    ;;                      ("go-github-com-golang-protobuf-1.5.2" ,go-github-com-golang-protobuf-1.5.2)
    ;;                      ("go-cloud-google-com-go-workflows-1.9.0" ,go-cloud-google-com-go-workflows-1.9.0)
    ;;                      ("go-cloud-google-com-go-websecurityscanner-1.4.0" ,go-cloud-google-com-go-websecurityscanner-1.4.0)
    ;;                      ("go-cloud-google-com-go-webrisk-1.7.0" ,go-cloud-google-com-go-webrisk-1.7.0)
    ;;                      ("go-cloud-google-com-go-vpcaccess-1.5.0" ,go-cloud-google-com-go-vpcaccess-1.5.0)
    ;;                      ("go-cloud-google-com-go-vmwareengine-0.1.0" ,go-cloud-google-com-go-vmwareengine-0.1.0)
    ;;                      ("go-cloud-google-com-go-vmmigration-1.3.0" ,go-cloud-google-com-go-vmmigration-1.3.0)
    ;;                      ("go-cloud-google-com-go-vision-v2-2.5.0" ,go-cloud-google-com-go-vision-v2-2.5.0)
    ;;                      ("go-cloud-google-com-go-videointelligence-1.9.0" ,go-cloud-google-com-go-videointelligence-1.9.0)
    ;;                      ("go-cloud-google-com-go-video-1.9.0" ,go-cloud-google-com-go-video-1.9.0)
    ;;                      ("go-cloud-google-com-go-translate-1.4.0" ,go-cloud-google-com-go-translate-1.4.0)
    ;;                      ("go-cloud-google-com-go-trace-1.4.0" ,go-cloud-google-com-go-trace-1.4.0)
    ;;                      ("go-cloud-google-com-go-tpu-1.4.0" ,go-cloud-google-com-go-tpu-1.4.0)
    ;;                      ("go-cloud-google-com-go-texttospeech-1.5.0" ,go-cloud-google-com-go-texttospeech-1.5.0)
    ;;                      ("go-cloud-google-com-go-talent-1.4.0" ,go-cloud-google-com-go-talent-1.4.0)
    ;;                      ("go-cloud-google-com-go-storagetransfer-1.6.0" ,go-cloud-google-com-go-storagetransfer-1.6.0)
    ;;                      ("go-cloud-google-com-go-speech-1.9.0" ,go-cloud-google-com-go-speech-1.9.0)
    ;;                      ("go-cloud-google-com-go-spanner-1.41.0" ,go-cloud-google-com-go-spanner-1.41.0)
    ;;                      ("go-cloud-google-com-go-shell-1.4.0" ,go-cloud-google-com-go-shell-1.4.0)
    ;;                      ("go-cloud-google-com-go-serviceusage-1.4.0" ,go-cloud-google-com-go-serviceusage-1.4.0)
    ;;                      ("go-cloud-google-com-go-servicemanagement-1.5.0" ,go-cloud-google-com-go-servicemanagement-1.5.0)
    ;;                      ("go-cloud-google-com-go-servicedirectory-1.7.0" ,go-cloud-google-com-go-servicedirectory-1.7.0)
    ;;                      ("go-cloud-google-com-go-servicecontrol-1.5.0" ,go-cloud-google-com-go-servicecontrol-1.5.0)
    ;;                      ("go-cloud-google-com-go-securitycenter-1.16.0" ,go-cloud-google-com-go-securitycenter-1.16.0)
    ;;                      ("go-cloud-google-com-go-security-1.10.0" ,go-cloud-google-com-go-security-1.10.0)
    ;;                      ("go-cloud-google-com-go-secretmanager-1.9.0" ,go-cloud-google-com-go-secretmanager-1.9.0)
    ;;                      ("go-cloud-google-com-go-scheduler-1.7.0" ,go-cloud-google-com-go-scheduler-1.7.0)
    ;;                      ("go-cloud-google-com-go-run-0.3.0" ,go-cloud-google-com-go-run-0.3.0)
    ;;                      ("go-cloud-google-com-go-retail-1.11.0" ,go-cloud-google-com-go-retail-1.11.0)
    ;;                      ("go-cloud-google-com-go-resourcesettings-1.4.0" ,go-cloud-google-com-go-resourcesettings-1.4.0)
    ;;                      ("go-cloud-google-com-go-resourcemanager-1.4.0" ,go-cloud-google-com-go-resourcemanager-1.4.0)
    ;;                      ("go-cloud-google-com-go-redis-1.10.0" ,go-cloud-google-com-go-redis-1.10.0)
    ;;                      ("go-cloud-google-com-go-recommender-1.8.0" ,go-cloud-google-com-go-recommender-1.8.0)
    ;;                      ("go-cloud-google-com-go-recommendationengine-0.6.0" ,go-cloud-google-com-go-recommendationengine-0.6.0)
    ;;                      ("go-cloud-google-com-go-recaptchaenterprise-v2-2.5.0" ,go-cloud-google-com-go-recaptchaenterprise-v2-2.5.0)
    ;;                      ("go-cloud-google-com-go-pubsublite-1.5.0" ,go-cloud-google-com-go-pubsublite-1.5.0)
    ;;                      ("go-cloud-google-com-go-pubsub-1.27.1" ,go-cloud-google-com-go-pubsub-1.27.1)
    ;;                      ("go-cloud-google-com-go-privatecatalog-0.6.0" ,go-cloud-google-com-go-privatecatalog-0.6.0)
    ;;                      ("go-cloud-google-com-go-policytroubleshooter-1.4.0" ,go-cloud-google-com-go-policytroubleshooter-1.4.0)
    ;;                      ("go-cloud-google-com-go-phishingprotection-0.6.0" ,go-cloud-google-com-go-phishingprotection-0.6.0)
    ;;                      ("go-cloud-google-com-go-oslogin-1.7.0" ,go-cloud-google-com-go-oslogin-1.7.0)
    ;;                      ("go-cloud-google-com-go-osconfig-1.10.0" ,go-cloud-google-com-go-osconfig-1.10.0)
    ;;                      ("go-cloud-google-com-go-orgpolicy-1.5.0" ,go-cloud-google-com-go-orgpolicy-1.5.0)
    ;;                      ("go-cloud-google-com-go-orchestration-1.4.0" ,go-cloud-google-com-go-orchestration-1.4.0)
    ;;                      ("go-cloud-google-com-go-optimization-1.2.0" ,go-cloud-google-com-go-optimization-1.2.0)
    ;;                      ("go-cloud-google-com-go-notebooks-1.5.0" ,go-cloud-google-com-go-notebooks-1.5.0)
    ;;                      ("go-cloud-google-com-go-networksecurity-0.6.0" ,go-cloud-google-com-go-networksecurity-0.6.0)
    ;;                      ("go-cloud-google-com-go-networkmanagement-1.5.0" ,go-cloud-google-com-go-networkmanagement-1.5.0)
    ;;                      ("go-cloud-google-com-go-networkconnectivity-1.7.0" ,go-cloud-google-com-go-networkconnectivity-1.7.0)
    ;;                      ("go-cloud-google-com-go-monitoring-1.8.0" ,go-cloud-google-com-go-monitoring-1.8.0)
    ;;                      ("go-cloud-google-com-go-metastore-1.8.0" ,go-cloud-google-com-go-metastore-1.8.0)
    ;;                      ("go-cloud-google-com-go-memcache-1.7.0" ,go-cloud-google-com-go-memcache-1.7.0)
    ;;                      ("go-cloud-google-com-go-mediatranslation-0.6.0" ,go-cloud-google-com-go-mediatranslation-0.6.0)
    ;;                      ("go-cloud-google-com-go-maps-0.1.0" ,go-cloud-google-com-go-maps-0.1.0)
    ;;                      ("go-cloud-google-com-go-managedidentities-1.4.0" ,go-cloud-google-com-go-managedidentities-1.4.0)
    ;;                      ("go-cloud-google-com-go-longrunning-0.3.0" ,go-cloud-google-com-go-longrunning-0.3.0)
    ;;                      ("go-cloud-google-com-go-logging-1.6.1" ,go-cloud-google-com-go-logging-1.6.1)
    ;;                      ("go-cloud-google-com-go-lifesciences-0.6.0" ,go-cloud-google-com-go-lifesciences-0.6.0)
    ;;                      ("go-cloud-google-com-go-language-1.8.0" ,go-cloud-google-com-go-language-1.8.0)
    ;;                      ("go-cloud-google-com-go-kms-1.6.0" ,go-cloud-google-com-go-kms-1.6.0)
    ;;                      ("go-cloud-google-com-go-iot-1.4.0" ,go-cloud-google-com-go-iot-1.4.0)
    ;;                      ("go-cloud-google-com-go-ids-1.2.0" ,go-cloud-google-com-go-ids-1.2.0)
    ;;                      ("go-cloud-google-com-go-iap-1.5.0" ,go-cloud-google-com-go-iap-1.5.0)
    ;;                      ("go-cloud-google-com-go-iam-0.8.0" ,go-cloud-google-com-go-iam-0.8.0)
    ;;                      ("go-cloud-google-com-go-gsuiteaddons-1.4.0" ,go-cloud-google-com-go-gsuiteaddons-1.4.0)
    ;;                      ("go-cloud-google-com-go-gkemulticloud-0.4.0" ,go-cloud-google-com-go-gkemulticloud-0.4.0)
    ;;                      ("go-cloud-google-com-go-gkehub-0.10.0" ,go-cloud-google-com-go-gkehub-0.10.0)
    ;;                      ("go-cloud-google-com-go-gkeconnect-0.6.0" ,go-cloud-google-com-go-gkeconnect-0.6.0)
    ;;                      ("go-cloud-google-com-go-gkebackup-0.3.0" ,go-cloud-google-com-go-gkebackup-0.3.0)
    ;;                      ("go-cloud-google-com-go-gaming-1.8.0" ,go-cloud-google-com-go-gaming-1.8.0)
    ;;                      ("go-cloud-google-com-go-functions-1.9.0" ,go-cloud-google-com-go-functions-1.9.0)
    ;;                      ("go-cloud-google-com-go-firestore-1.9.0" ,go-cloud-google-com-go-firestore-1.9.0)
    ;;                      ("go-cloud-google-com-go-filestore-1.4.0" ,go-cloud-google-com-go-filestore-1.4.0)
    ;;                      ("go-cloud-google-com-go-eventarc-1.8.0" ,go-cloud-google-com-go-eventarc-1.8.0)
    ;;                      ("go-cloud-google-com-go-essentialcontacts-1.4.0" ,go-cloud-google-com-go-essentialcontacts-1.4.0)
    ;;                      ("go-cloud-google-com-go-errorreporting-0.3.0" ,go-cloud-google-com-go-errorreporting-0.3.0)
    ;;                      ("go-cloud-google-com-go-edgecontainer-0.2.0" ,go-cloud-google-com-go-edgecontainer-0.2.0)
    ;;                      ("go-cloud-google-com-go-domains-0.7.0" ,go-cloud-google-com-go-domains-0.7.0)
    ;;                      ("go-cloud-google-com-go-documentai-1.10.0" ,go-cloud-google-com-go-documentai-1.10.0)
    ;;                      ("go-cloud-google-com-go-dlp-1.7.0" ,go-cloud-google-com-go-dlp-1.7.0)
    ;;                      ("go-cloud-google-com-go-dialogflow-1.19.0" ,go-cloud-google-com-go-dialogflow-1.19.0)
    ;;                      ("go-cloud-google-com-go-deploy-1.5.0" ,go-cloud-google-com-go-deploy-1.5.0)
    ;;                      ("go-cloud-google-com-go-datastream-1.5.0" ,go-cloud-google-com-go-datastream-1.5.0)
    ;;                      ("go-cloud-google-com-go-datastore-1.10.0" ,go-cloud-google-com-go-datastore-1.10.0)
    ;;                      ("go-cloud-google-com-go-dataqna-0.6.0" ,go-cloud-google-com-go-dataqna-0.6.0)
    ;;                      ("go-cloud-google-com-go-dataproc-1.8.0" ,go-cloud-google-com-go-dataproc-1.8.0)
    ;;                      ("go-cloud-google-com-go-dataplex-1.4.0" ,go-cloud-google-com-go-dataplex-1.4.0)
    ;;                      ("go-cloud-google-com-go-datalabeling-0.6.0" ,go-cloud-google-com-go-datalabeling-0.6.0)
    ;;                      ("go-cloud-google-com-go-datafusion-1.5.0" ,go-cloud-google-com-go-datafusion-1.5.0)
    ;;                      ("go-cloud-google-com-go-dataform-0.5.0" ,go-cloud-google-com-go-dataform-0.5.0)
    ;;                      ("go-cloud-google-com-go-dataflow-0.7.0" ,go-cloud-google-com-go-dataflow-0.7.0)
    ;;                      ("go-cloud-google-com-go-datacatalog-1.8.0" ,go-cloud-google-com-go-datacatalog-1.8.0)
    ;;                      ("go-cloud-google-com-go-containeranalysis-0.6.0" ,go-cloud-google-com-go-containeranalysis-0.6.0)
    ;;                      ("go-cloud-google-com-go-container-1.7.0" ,go-cloud-google-com-go-container-1.7.0)
    ;;                      ("go-cloud-google-com-go-contactcenterinsights-1.4.0" ,go-cloud-google-com-go-contactcenterinsights-1.4.0)
    ;;                      ("go-cloud-google-com-go-compute-1.13.0" ,go-cloud-google-com-go-compute-1.13.0)
    ;;                      ("go-cloud-google-com-go-cloudtasks-1.8.0" ,go-cloud-google-com-go-cloudtasks-1.8.0)
    ;;                      ("go-cloud-google-com-go-clouddms-1.4.0" ,go-cloud-google-com-go-clouddms-1.4.0)
    ;;                      ("go-cloud-google-com-go-cloudbuild-1.4.0" ,go-cloud-google-com-go-cloudbuild-1.4.0)
    ;;                      ("go-cloud-google-com-go-channel-1.9.0" ,go-cloud-google-com-go-channel-1.9.0)
    ;;                      ("go-cloud-google-com-go-certificatemanager-1.4.0" ,go-cloud-google-com-go-certificatemanager-1.4.0)
    ;;                      ("go-cloud-google-com-go-binaryauthorization-1.4.0" ,go-cloud-google-com-go-binaryauthorization-1.4.0)
    ;;                      ("go-cloud-google-com-go-billing-1.7.0" ,go-cloud-google-com-go-billing-1.7.0)
    ;;                      ("go-cloud-google-com-go-bigquery-1.44.0" ,go-cloud-google-com-go-bigquery-1.44.0)
    ;;                      ("go-cloud-google-com-go-beyondcorp-0.3.0" ,go-cloud-google-com-go-beyondcorp-0.3.0)
    ;;                      ("go-cloud-google-com-go-batch-0.4.0" ,go-cloud-google-com-go-batch-0.4.0)
    ;;                      ("go-cloud-google-com-go-baremetalsolution-0.4.0" ,go-cloud-google-com-go-baremetalsolution-0.4.0)
    ;;                      ("go-cloud-google-com-go-automl-1.8.0" ,go-cloud-google-com-go-automl-1.8.0)
    ;;                      ("go-cloud-google-com-go-assuredworkloads-1.9.0" ,go-cloud-google-com-go-assuredworkloads-1.9.0)
    ;;                      ("go-cloud-google-com-go-asset-1.10.0" ,go-cloud-google-com-go-asset-1.10.0)
    ;;                      ("go-cloud-google-com-go-artifactregistry-1.9.0" ,go-cloud-google-com-go-artifactregistry-1.9.0)
    ;;                      ("go-cloud-google-com-go-area120-0.6.0" ,go-cloud-google-com-go-area120-0.6.0)
    ;;                      ("go-cloud-google-com-go-appengine-1.5.0" ,go-cloud-google-com-go-appengine-1.5.0)
    ;;                      ("go-cloud-google-com-go-apigeeconnect-1.4.0" ,go-cloud-google-com-go-apigeeconnect-1.4.0)
    ;;                      ("go-cloud-google-com-go-apigateway-1.4.0" ,go-cloud-google-com-go-apigateway-1.4.0)
    ;;                      ("go-cloud-google-com-go-analytics-0.12.0" ,go-cloud-google-com-go-analytics-0.12.0)
    ;;                      ("go-cloud-google-com-go-aiplatform-1.27.0" ,go-cloud-google-com-go-aiplatform-1.27.0)
    ;;                      ("go-cloud-google-com-go-accesscontextmanager-1.4.0" ,go-cloud-google-com-go-accesscontextmanager-1.4.0)
    ;;                      ("go-cloud-google-com-go-accessapproval-1.5.0" ,go-cloud-google-com-go-accessapproval-1.5.0)
    ;;                      ("go-cloud-google-com-go-0.105.0" ,go-cloud-google-com-go-0.105.0)))
    (home-page "https://google.golang.org/genproto")
    (synopsis "Go generated proto packages")
    (description
     "This repository contains the generated Go packages for common protocol buffer
types, and the generated @@url{http://grpc.io,gRPC} code necessary for
interacting with Google's gRPC APIs.")
    (license license:asl2.0)))

(define-public go-google-golang-org-grpc-1.53.0
  (package
    (name "go-google-golang-org-grpc")
    (version "1.53.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grpc/grpc-go")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xln232623q55wlzb1f4754006xw8mkm5wqynsdrklpnj7m00lca"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "google.golang.org/grpc"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-google-golang-org-appengine-1.6.7" ,go-google-golang-org-appengine-1.6.7)
    ;;                      ("go-golang-org-x-text-0.6.0" ,go-golang-org-x-text-0.6.0)
    ;;                      ("go-github-com-envoyproxy-protoc-gen-validate-0.9.1" ,go-github-com-envoyproxy-protoc-gen-validate-0.9.1)
    ;;                      ("go-github-com-census-instrumentation-opencensus-proto-0.4.1" ,go-github-com-census-instrumentation-opencensus-proto-0.4.1)
    ;;                      ("go-cloud-google-com-go-compute-metadata-0.2.3" ,go-cloud-google-com-go-compute-metadata-0.2.3)
    ;;                      ("go-cloud-google-com-go-compute-1.15.1" ,go-cloud-google-com-go-compute-1.15.1)
    ;;                      ("go-google-golang-org-protobuf-1.28.1" ,go-google-golang-org-protobuf-1.28.1)
    ;;                      ("go-google-golang-org-genproto-0.0.0-20230110181048-76db0878b65f" ,go-google-golang-org-genproto-0.0.0-20230110181048-76db0878b65f)
    ;;                      ("go-golang-org-x-sys-0.4.0" ,go-golang-org-x-sys-0.4.0)
    ;;                      ("go-golang-org-x-oauth2-0.4.0" ,go-golang-org-x-oauth2-0.4.0)
    ;;                      ("go-golang-org-x-net-0.5.0" ,go-golang-org-x-net-0.5.0)
    ;;                      ("go-github-com-google-uuid-1.3.0" ,go-github-com-google-uuid-1.3.0)
    ;;                      ("go-github-com-google-go-cmp-0.5.9" ,go-github-com-google-go-cmp-0.5.9)
    ;;                      ("go-github-com-golang-protobuf-1.5.2" ,go-github-com-golang-protobuf-1.5.2)
    ;;                      ("go-github-com-golang-glog-1.0.0" ,go-github-com-golang-glog-1.0.0)
    ;;                      ("go-github-com-envoyproxy-go-control-plane-0.10.3" ,go-github-com-envoyproxy-go-control-plane-0.10.3)
    ;;                      ("go-github-com-cncf-xds-go-0.0.0-20230105202645-06c439db220b" ,go-github-com-cncf-xds-go-0.0.0-20230105202645-06c439db220b)
    ;;                      ("go-github-com-cncf-udpa-go-0.0.0-20220112060539-c52dc94e7fbe" ,go-github-com-cncf-udpa-go-0.0.0-20220112060539-c52dc94e7fbe)
    ;;                      ("go-github-com-cespare-xxhash-v2-2.2.0" ,go-github-com-cespare-xxhash-v2-2.2.0)))
    (home-page "https://google.golang.org/grpc")
    (synopsis "gRPC-Go")
    (description "Package grpc implements an RPC system called gRPC.")
    (license license:asl2.0)))

(define-public go-google-golang-org-protobuf-1.28.1
  (package
    (name "go-google-golang-org-protobuf")
    (version "1.28.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qy9wy36wr1vj8lhmzi26hfc14y3rfbsi0p4vkbxhiwb3iy3na7c"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "google.golang.org/protobuf"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-github-com-google-go-cmp-0.5.5" ,go-github-com-google-go-cmp-0.5.5)
    ;;                      ("go-github-com-golang-protobuf-1.5.0" ,go-github-com-golang-protobuf-1.5.0)))
    (home-page "https://google.golang.org/protobuf")
    (synopsis "Go support for Protocol Buffers")
    (description
     "This project hosts the Go implementation for @@url{https://protobuf.dev,protocol
buffers}, which is a language-neutral, platform-neutral, extensible mechanism
for serializing structured data.  The protocol buffer language is a language for
specifying the schema for structured data.  This schema is compiled into
language specific bindings.  This project provides both a tool to generate Go
code for the protocol buffer language, and also the runtime implementation to
handle serialization of messages in Go.  See the
@@url{https://protobuf.dev/overview,protocol buffer developer guide} for more
information about protocol buffers themselves.")
    (license license:bsd-3)))

(define-public go-gopkg-in-check-v1-1.0.0-20190902080502-41f04d3bba15
  (package
    (name "go-gopkg-in-check-v1")
    (version "1.0.0-20190902080502-41f04d3bba15")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gopkg.in/check.v1")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vfk9czmlxmp6wndq8k17rhnjxal764mxfhrccza7nwlia760pjy"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/check.v1"
       #:unpack-path "gopkg.in/check.v1"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    (home-page "https://gopkg.in/check.v1")
    (synopsis "Instructions")
    (description
     "Package check is a rich testing extension for Go's testing package.")
    (license license:bsd-2)))

(define-public go-gopkg-in-yaml-v2-2.2.8
  (package
    (name "go-gopkg-in-yaml-v2")
    (version "2.2.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gopkg.in/yaml.v2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1inf7svydzscwv9fcjd2rm61a4xjk6jkswknybmns2n58shimapw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/yaml.v2"
       #:unpack-path "gopkg.in/yaml.v2"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
                        ;; Source-only package
                        (delete 'build))))
    ;; (propagated-inputs `(("go-gopkg-in-check-v1-0.0.0-20161208181325-20d25e280405" ,go-gopkg-in-check-v1-0.0.0-20161208181325-20d25e280405)))
    (home-page "https://gopkg.in/yaml.v2")
    (synopsis "YAML support for the Go language")
    (description "Package yaml implements YAML support for the Go language.")
    (license license:asl2.0)))

(define-public go-github-com-dgraph-io-badger-4.2.0
  (package
    (name "go-github-com-dgraph-io-badger")
    (version "4.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dgraph-io/badger")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0150kqkzqrszbfdprkdqnrbdzr9bj16xapwx0yqsi6h6cm69bgzr"))))
    (build-system go-build-system)
    (inputs (list jemalloc-alt))
    (arguments
     `(#:import-path "github.com/dgraph-io/badger"
       #:go ,go-1.19
       #:build-flags '("--tags=jemalloc")))
    (propagated-inputs (list go-github-com-cespare-xxhash-v2-2.2.0
                             go-github-com-dgraph-io-ristretto-0.1.1
                             go-github-com-dustin-go-humanize-1.0.0
                             go-github-com-gogo-protobuf-1.3.2
                             go-github-com-golang-protobuf-1.5.2
                             go-github-com-golang-snappy-0.0.3
                             go-github-com-google-flatbuffers-1.12.1
                             go-github-com-klauspost-compress-1.12.3
                             go-github-com-pkg-errors-0.9.1
                             go-github-com-spf13-cobra-0.0.5
                             go-github-com-stretchr-testify-1.4.0
                             go-go-opencensus-io-0.22.5
                             go-golang-org-x-net-0.7.0
                             go-golang-org-x-sys-0.5.0
                                        ;indirect
                             go-github-com-davecgh-go-spew-1.1.1
                             go-github-com-golang-glog-1.0.0
                             go-github-com-golang-groupcache-0.0.0-20190702054246-869f871628b6
                             go-github-com-inconshreveable-mousetrap-1.0.0
                             go-github-com-kr-pretty-0.1.0
                             go-github-com-pmezard-go-difflib-1.0.0
                             go-github-com-spf13-pflag-1.0.3
                             go-google-golang-org-genproto-0.0.0-20230110181048-76db0878b65f
                             go-google-golang-org-grpc-1.53.0
                             go-google-golang-org-protobuf-1.28.1
                             go-gopkg-in-check-v1-1.0.0-20190902080502-41f04d3bba15
                             go-gopkg-in-yaml-v2-2.2.8))
    (home-page "https://github.com/dgraph-io/badger")
    (synopsis "BadgerDB")
    (description
     "Package badger implements an embeddable, simple and fast key-value database,
written in pure Go.  It is designed to be highly performant for both reads and
writes simultaneously.  Badger uses Multi-Version Concurrency Control (MVCC),
and supports transactions.  It runs transactions concurrently, with serializable
snapshot isolation guarantees.")
    (license license:asl2.0)))

(define-public badger-4.2.0
  (package
   (name "badger")
   (version "4.2.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/dgraph-io/badger")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0150kqkzqrszbfdprkdqnrbdzr9bj16xapwx0yqsi6h6cm69bgzr"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/dgraph-io/badger/badger"
      #:unpack-path "github.com/dgraph-io/badger"
      #:install-source? #f
      #:go ,go-1.19
      #:build-flags '("--tags=jemalloc")
      #:phases
      (modify-phases %standard-phases
        (delete 'unpack))))
   (inputs (list jemalloc-alt))
   (propagated-inputs (list go-github-com-dgraph-io-badger-4.2.0))
   (home-page "https://github.com/dgraph-io/badger")
   (synopsis "BadgerDB")
   (description
    "Package badger implements an embeddable, simple and fast key-value database,
written in pure Go.  It is designed to be highly performant for both reads and
writes simultaneously.  Badger uses Multi-Version Concurrency Control (MVCC),
and supports transactions.  It runs transactions concurrently, with serializable
snapshot isolation guarantees.")
   (license license:asl2.0)))
