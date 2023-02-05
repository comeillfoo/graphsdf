SHELL:=/bin/bash

RKT?=racket
RACO?=raco
FMT?=fmt
REVIEW?=review
TEST?=test


TESTSDIR=tests
SRCDIR=lang


SOURCES=$(wildcard $(SRCDIR)/*.rkt)
SOURCES+=$(wildcard $(TESTSDIR)/*.rkt)
SOURCES+=backend.rkt


format:
	diff -u <(cat $(SOURCES)) <($(RACO) $(FMT) $(SOURCES))


format-fix:
	$(RACO) $(FMT) -i $(SOURCES)


$(SOURCES):
	$(RACO) $(REVIEW) $@


lint: $(SOURCES)


test:
	$(RACO) $(TEST) $(filter-out backend.rkt,$(SOURCES))


.PHONY: test lint format format-fix $(SOURCES)