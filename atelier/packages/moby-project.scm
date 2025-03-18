(define-module (atelier packages moby-project)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public moby-thesaurus-ii
  (package
   (name "moby-thesaurus-ii")
   (version "3202")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.gutenberg.org/dirs/3/2/0/" version "/files.zip"))
            (sha256 (base32 "1n56702si4zxl18a6dhkbm8ayfg9l6yi6c8cw09k65dlscfycm1s"))))
   (build-system copy-build-system)
   (native-inputs (list (specification->package (compressor (origin-uri source)))))
   (synopsis "Moby Thesaurus for the MSDOS operating system")
   (description "Moby™ Thesaurus for the MSDOS operating system is compressed and
distributed as a single zip file.  After extraction, the vocabulary
files included with this product are in ordinary ASCII format with
CRLF (ASCII 13/10) delimiters.")
   (home-page (string-append "https://www.gutenberg.org/ebooks/" version))
   (license license:public-domain)))




