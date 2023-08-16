(define-module (atelier packages hugin)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module ((gnu packages photo) #:prefix gnu:)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg))

(define glew-with-egl ;; Patched glew with EGL support for Hugin
  (package
   (inherit glew)
   (version (package-version glew))
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/glew/glew/" version
                                "/glew-" version ".tgz"))
            (sha256
             (base32
              "1qak8f7g1iswgswrgkzc7idk7jmqgwrs58fhg2ai007v7j4q5z6l"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                (substitute* "config/Makefile.linux"
			     (("= cc")          "= gcc")
			     (("/lib64")        "/lib")
			     (("LDFLAGS.GL = ") "LDFLAGS.GL = -lEGL ")
			     (("BIN.SUFFIX =")  "CFLAGS.EXTRA += -DGLEW_EGL\nBIN.SUFFIX ="))
                #t))))
   (inputs (modify-inputs (package-inputs glew)
                          (append eglexternalplatform)))))

(define-public hugin
  (package
   (inherit gnu:hugin)
   (inputs (modify-inputs (package-inputs gnu:hugin)
                          (replace "glew" glew-with-egl)))))

(define-public hugin-2022.0.0
  (package
   (inherit hugin)
   (version "2022.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/hugin/hugin/hugin-"
                                (version-major+minor version)
                                "/hugin-" version ".tar.bz2"))
            (sha256
             (base32
              "19q5drj826nyd4myxwp5v46cdca8w3ykv92mp7h479x91cm5dj4p"))))))
