emacs ?= emacs

LOAD = -l mines.el

all: test

test:
	$(emacs) -batch $(LOAD) -l mines-tests.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -batch --eval "(progn (add-to-list 'load-path default-directory) (byte-compile-file \"mines.el\"))"

clean:
	rm -f *.elc

.PHONY: all compile clean test
