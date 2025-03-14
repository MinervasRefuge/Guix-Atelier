(define-module (atelier packages emacs)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix build-system emacs)
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


