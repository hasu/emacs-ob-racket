byte-compile-racket :
	raco make ob-racket-runtime.rkt

byte-compile-elisp :
	emacs --eval '(byte-compile-file "ob-racket.el")'

clean :
	-rm -r compiled
	-rm *.elc
