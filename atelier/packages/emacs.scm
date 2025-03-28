(define-module (atelier packages emacs)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-separedit
  (let ((commit "0001d2b4fcc6e5f40ebc82dd3c80ac7ce4031234")
        (revision "0"))
    (package
     (name "emacs-separedit")
     (version (git-version "0.3.37" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twlz0ne/separedit.el")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256 (base32 "14cs3vjfs077w5v1vnhddi8a7xag2x8b9vam8pdlcn48737bk7mw"))))
     (propagated-inputs
      (list emacs-dash emacs-edit-indirect))
     (build-system emacs-build-system)
     (synopsis "Edit comment/string/docstring/code block in separate buffer with your favorite mode.")
     (description synopsis)
     (home-page "https://github.com/twlz0ne/separedit.el")
     (license license:gpl3))))

(define emacs-org/testing
  (package
   (inherit emacs-org)
   (name "emacs-org-testing")
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     ''(("testing/" "share/emacs/site-lisp"))))))

(define-public emacs-ob-zig
  (let ((commit "1913fae27caa0be2572a7c501a683ad51918c26c")
        (revision "0"))
    (package
     (name "emacs-ob-zig")
     (version (git-version "0.0.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jolby/ob-zig.el")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256 (base32 "1jhkw4d19nzfi61grhmgw6bds4bi4sxg9fh7krggmxw823a3g3nf"))))
     (build-system emacs-build-system)
     (arguments '(#:tests? #f
                  #:test-command '("emacs" "--batch"
                                   "--load" "ob-zig.el"
                                   "--load" "test-ob-zig.el"
                                   "--load" "test-ob-zig-runner.el"
                                   "--eval" "(test-ob-zig-run-tests)")
                  #:exclude (cons ".*test.*"  %default-exclude)
                  ))
     ;;(native-inputs (list emacs-org/testing)) ;; just for testing
     (propagated-inputs (list emacs-zig-mode))
     (synopsis "Org Babel bindings for the Zig programming language")
     (description "ob-zig.el provides Org Babel support for the Zig programming language.
Org Babel allows you to interactively edit and execute source code for many different
languages in the same document.

This implementation mostly follows ob-C.el, which provides org-babel support for the
C/C++/D languages.")
     (home-page "https://github.com/jolby/ob-zig.el")
     (license license:gpl3+))))


