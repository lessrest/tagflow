# Minimal Makefile for cl-tagflow

LISP := sbcl

.PHONY: test repl clean

test:; @$(LISP) --noinform --non-interactive --load boot.lisp \
  --eval '(asdf:test-system "tagflow")'

repl:; @rlwrap $(LISP) --load boot.lisp
clean:; @rm -f *.fasl
