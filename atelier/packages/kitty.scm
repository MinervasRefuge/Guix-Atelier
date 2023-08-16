(define-module (atelier packages kitty)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system go)
  #:use-module (guix build go-build-system)
  #:use-module (gnu packages python-build)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages image)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages node)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages shells))

;; todo: Doesn't belong here
(define-public python-sphinx-inline-tabs
  (package
   (name "python-sphinx-inline-tabs")
   (version "2023.4.21")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "sphinx_inline_tabs" version))
            (sha256
             (base32
              "1g5yhdk208i8maippnbnijd1knpai809wl3cbwzqy59cc0zz3wjx"))))
   (build-system pyproject-build-system)
   (arguments `(#:tests? #f))
   (propagated-inputs
    (list python-docutils python-pygments python-sphinx python-flit-core))
   (home-page "https://github.com/pradyunsg/sphinx-inline-tabs")
   (synopsis "Add inline tabbed content to your Sphinx documentation.")
   (description "Add inline tabbed content to your Sphinx documentation.")
   (license #f)))

;; Last kitty version that doesn't use golang
(define-public kitty-0.26.5
  (package
   (inherit kitty)
   (name "kitty")
   (version "0.26.5")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/kovidgoyal/kitty")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "07n0wvj4jfdqnrh4a12c9z7zp3iwwllkgzrkdwppj7msbnah2njj"))
     (modules '((guix build utils)))
     (snippet
      '(begin
         ;; patch needed as sphinx-build is used as a python script
         ;; whereas the guix package uses a bash script launching the
         ;; python script
         (substitute* "docs/conf.py"
                      (("(from kitty.constants import str_version)" kitty-imp)
                       (string-append "sys.path.append(\"..\")\n" kitty-imp))
                      (("html_theme = 'furo'")
                        "html_theme = 'classic'"))
         (substitute* "docs/Makefile"
                      (("^SPHINXBUILD[[:space:]]+= (python3.*)$")
                       "SPHINXBUILD = sphinx-build\n"))
         ;; (substitute* "kitty_tests/check_build.py"
         ;;              (("(^    def test_docs_url\\(self\\):)" m)
         ;;               (string-append "    @unittest.skip('Containered env prevents web access')\n" m)))
         (substitute* "kitty_tests/prewarm.py"
                      (("(import time)" m)
                       (string-append m "\nimport unittest"))
                      (("(^    def test_signal_handling\\(self\\):)" m)
                       (string-append "    @unittest.skip('Fails in containered env')\n" m)))
         #t))))
   (inputs (modify-inputs (package-inputs kitty)
                          (append librsync
                                  python-sphinx-copybutton
                                  python-sphinx-inline-tabs
                                  python-sphinxext-opengraph
                                  openssl
                                  bash
                                  ;; tests
                                  ;fish ; fails
                                  zsh)))
   (native-inputs (modify-inputs (package-native-inputs kitty)
                          (append fontconfig)))
   (arguments
    (list #:phases
          #~(modify-phases %standard-phases
                           (delete 'configure) ;no configure script
                           (replace 'build
                                    (lambda* (#:key inputs #:allow-other-keys)
                                      ;; The "kitty" sub-directory must be writable prior to
                                      ;; configuration (e.g., un-setting updates).
                                      (for-each make-file-writable (find-files "kitty"))
                                      (invoke "python3" "setup.py" "linux-package"
                                              ;; Do not phone home.
                                              "--update-check-interval=0"
                                              ;; Wayland backend requires EGL, which isn't
                                              ;; found out-of-the-box for some reason.
                                           
                                              (string-append "--egl-library="
                                                             (search-input-file inputs "/lib/libEGL.so.1"))
                                              (string-append "--fontconfig-library="
                                                             (search-input-file inputs "/lib/libfontconfig.so"))
                                              ;; (string-append "--fontconfig-library="
                                              ;;                (search-input-file inputs "/lib/libcanberra.so"))
                                              "--ignore-compiler-warnings")))
                           (replace 'check
                                    (lambda* (#:key tests? #:allow-other-keys)
                                      (when tests?
                                        ;; is this really desirable?
                                        ;; It's not the build directory (might default on old cached dirs if
                                        ;; `guix build -K` is used)
                                        (setenv "PYTHONPATH" (getcwd))
                                        (invoke "linux-package/bin/kitty" "+launch" "test.py"))))
                           (add-before 'install 'rm-pycache
                                       ;; created python cache __pycache__ are non deterministic
                                       (lambda _
                                         (let ((pycaches (find-files "linux-package/"
                                                                     "__pycache__"
                                                                     #:directories? #t)))
                                           (for-each delete-file-recursively pycaches))))
                           (replace 'install
                                    (lambda _
                                      (let* ((obin (string-append #$output "/bin"))
                                             (olib (string-append #$output "/lib"))
                                             (oshare (string-append #$output "/share")))
                                        (copy-recursively "linux-package/bin" obin)
                                        (copy-recursively "linux-package/share" oshare)
                                        (copy-recursively "linux-package/lib" olib)))))))))
