(use-modules (guix packages)
	     (guix download)
	     (guix utils)
	     (guix gexp)
	     (srfi srfi-1)
	     (gnu packages photo)
	     (gnu packages wxwidgets)
	     (gnu packages gl)
	     (gnu packages xorg))

(define alt-glew ;; Patched glew with EGL support for Hugin
  (package
   (inherit glew)
   (version "2.2.0")
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
   (inputs (cons `("eglexternalplatform" ,eglexternalplatform)
		  (package-inputs glew)))))

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
            "19q5drj826nyd4myxwp5v46cdca8w3ykv92mp7h479x91cm5dj4p"))))
 (inputs (let ((original (list-copy (package-inputs hugin))))
	   (assoc-remove! original "glew")
	   (cons* `("glew" ,alt-glew) original))))
