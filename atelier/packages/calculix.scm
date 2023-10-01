(define-module (atelier packages calculix)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:))

;;todo documentation fix/split
(define-public spooles-2.2
  (package
    (name "spooles")
    (version "2.2")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "https://www.netlib.org/linalg/spooles/spooles." version ".tgz"))
	      (sha256
	       (base32 "1pf5z3vvwd8smbpibyabprdvcmax0grzvx2y0liy98c7x6h5jid8"))))
    (build-system gnu-build-system)
    (inputs (list tcsh
		  perl))
    (arguments
     `(#:make-flags (list "lib") ;;looks like you could have called `global` but appears to be broken so *shrug*
       #:phases
       (modify-phases %standard-phases
	 (delete 'configure)            ;no configure script
	 (delete 'check)
	 (add-before 'build 'fix-make
	   (lambda _
	     (chdir "../")
	     (substitute* "Make.inc"
	       (("  CC = /usr/lang-4.0/bin/cc")
		"  CC = gcc")
	       (("  CFLAGS = \\$\\(OPTLEVEL\\)")
		"  CFLAGS = -fPIC $(OPTLEVEL)"))
	     (substitute* "makefile"
	       (("(\tcd misc             ; make lib)" m1)
		(string-append m1
			       "\n"
			       "\tar -x spooles.a\n"
			       "\tgcc -shared *.o -o libspooles.so"
			       )))))
	 (replace 'install
	   (lambda _
	     (let* ((out-dir (assoc-ref %outputs "out"))
		    (lib-dir (string-append out-dir "/lib"))
		    (inc-dir (string-append out-dir "/include")))
	       (mkdir-p lib-dir)
	       (copy-file "libspooles.so"
			  (string-append lib-dir
					 "/libspooles.so"))
	       (mkdir-p inc-dir)
	       (for-each (lambda (p)
			   (mkdir-p (string-append inc-dir "/" (dirname p)))
			   (copy-file p
				      (string-append inc-dir "/" p)))
			 (find-files "." "\\.h$"))))))))
    (home-page "https://netlib.org/linalg/spooles/spooles.2.2.html")
    (synopsis "SPOOLES is a library for solving sparse real and complex linear systems of equations, written in the C language using object oriented design.")
    (description "SPOOLES is a library for solving sparse real and complex linear systems of equations, written in the C language using object oriented design. At present, there is the following functionality:

    Compute multiple minimum degree, generalized nested dissection and multisection orderings of matrices with symmetric structure.

    Factor and solve square linear systems of equations with symmetric structure, with or without pivoting for stability. The factorization can be symmetric LDLT, Hermitian LDLH, or nonsymmetric LDU. A direct factorization or a drop tolerance factorization can be computed. The factors and solve can be done in serial mode, multithreaded with Solaris or POSIX threads, or with MPI.

    Factor and solve overdetermined full rank systems of equations using a multifrontal QR factorization, in serial or using POSIX threads.

    Solve square linear systems using a variety of Krylov iterative methods. The preconditioner is a drop tolerance factorization, constructed with or without pivoting for stability.")
    (license license:public-domain))) 

(define-public ccx-prool-175dc0f
  (package
    (name "ccx-prool")
    (version "175dc0f")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/calculix/ccx_prool")
		    (commit version)))
	      (sha256
	       (base32 "1i70xd19prs3b34a711vasrqr3i1kjwyvhzm4x8qql9jkr89shh2"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                            '("SPOOLES.2.2" "ARPACK" "ARPACK_i8"))
                  (substitute* "CalculiX/ccx_2.21/src/Makefile"
                    (("CC=cc")
                     "CC=gcc")
                    (("-I \\.\\./\\.\\./\\.\\./SPOOLES\\.2\\.2")
                     "-fPIC")
                    (("FFLAGS = -Wall -O2")
                     "FFLAGS = -fPIC -Wall -O2")
                    (("\\$\\(DIR\\)/spooles\\.a")
                     "-lspooles")
                    (("\\.\\./\\.\\./\\.\\./ARPACK/libarpack_INTEL\\.a")
                     "-larpack -lblas -llapack")
                    (("ccx_2\\.21: \\$\\(OCCXMAIN\\) ccx_2\\.21\\.a  \\$\\(LIBS\\)")
                     "ccx_2.21: $(OCCXMAIN) ccx_2.21.a")
                    (("(ar vr \\$@ \\$\\?)" m1)
                     (string-append m1 "\n\trm ccx_2.21.o; gcc -shared *.o -o libccx.so")))))))
    (build-system gnu-build-system)
    (inputs (list gfortran-toolchain perl))
    (native-inputs (list arpack-ng
                         spooles-2.2
                         openblas
                         lapack))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
	 (delete 'configure) ;no configure script
         (delete 'check)
         (add-before 'build 'chdir
           (lambda _ (chdir "CalculiX/ccx_2.21/src")))
         (replace 'install
           (lambda _
             (let* ((out-dir (assoc-ref %outputs "out"))
                    (bin-dir (string-append out-dir "/bin"))
                    (lib-dir (string-append out-dir "/lib")))
               (mkdir-p bin-dir)
               (mkdir-p lib-dir)
               (copy-file "ccx_2.21" (string-append bin-dir "/ccx"))
               (copy-file "libccx.so" (string-append lib-dir "/libccx.so"))))))))
    (home-page "http://www.dhondt.de/")
    (synopsis "A Free Software Three-Dimensional Structural Finite Element Program")
    (description "")
    (license license:gpl2))) 

(define-public cgx-prool-774e4f5
  (package
    (name "cgx-prool")
    (version "774e4f5")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/calculix/cgx_prool")
		    (commit version)))
	      (sha256
	       (base32 "1vahbx6qw1xhlkzd7snzg593hmdwgi25rh7y88pm5iijvkknls7f"))
	      (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "cgx_2.21/src/Makefile"
			       (("(# on 32bit systems)" m1)
				(string-append "CC=gcc\n" m1)))))))
    (build-system gnu-build-system)
    (inputs (list freeglut))
    (native-inputs (list libxmu
			 libxi
			 libxt))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
	 (delete 'configure) ;no configure script
         (delete 'check)
	 (add-before 'build 'chdir (lambda _ (chdir "cgx_2.21/src")))
	 (replace 'install
           (lambda _
             (let* ((out-dir (assoc-ref %outputs "out"))
                    (bin-dir (string-append out-dir "/bin")))
               (mkdir-p bin-dir)
               (copy-file "cgx" (string-append bin-dir "/cgx"))))))))
    (home-page "http://www.dhondt.de/")
    (synopsis "A Free Software Three-Dimensional Structural Finite Element Program")
    (description "")
    (license license:gpl2))) 
