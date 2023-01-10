SHELL:=/bin/bash

RKT?=racket
RACO?=raco
FMT?=fmt
REVIEW?=review
TEST?=test


TESTSDIR=tests
SRCDIR=lang


SOURCES=$(filter-out $(SRCDIR)/parser.rkt,$(wildcard $(SRCDIR)/*.rkt))
SOURCES+=$(filter-out $(TESTSDIR)/parser-test.rkt,$(wildcard $(TESTSDIR)/*.rkt))
SOURCES+=backend.rkt


format:
	diff -u <(cat $(SOURCES)) <($(RACO) $(FMT) $(SOURCES))


format-fix:
	$(RACO) $(FMT) -i $(SOURCES)


$(SOURCES):
	$(RACO) $(REVIEW) $@


lint: $(SOURCES)


test:
	$(RKT) $(TESTSDIR)/parser-test.rkt
	$(RACO) $(TEST) $(filter-out backend.rkt,$(SOURCES))


.PHONY: test lint format format-fix $(SOURCES)