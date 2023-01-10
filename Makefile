SHELL:=/bin/bash

RACO?=raco
FMT?=fmt
REVIEW?=review
TEST?=test

TESTS=tests
SRC=.

SOURCES=$(filter-out $(SRC)/parser.rkt,$(wildcard $(SRC)/*.rkt))
SOURCES+=$(filter-out $(TESTS)/parser-test.rkt,$(wildcard $(TESTS)/*.rkt))


format:
	diff -u <(cat $(SOURCES)) <($(RACO) $(FMT) $(SOURCES))


format-fix:
	$(RACO) $(FMT) -i $(SOURCES)


$(SOURCES):
	$(RACO) $(REVIEW) $@


lint: $(SOURCES)


test:
	$(RACO) $(TEST) $(SRC)


.PHONY: test lint format format-fix $(SOURCES)