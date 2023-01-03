SHELL:=/bin/bash

RACO?=raco
FMT?=fmt
REVIEW?=review
EXE?=exe
DISTR?=distribute
TEST?=test

TESTS=tests
RKTDIR=.

racket-format:
	diff -u <(cat $(RKTDIR)/*.rkt ) <($(RACO) $(FMT) $(RKTDIR)/*.rkt )


racket-format-fix:
	find $(RKTDIR) -type f -name '*.rkt' -print0 | xargs -0 -n1 $(RACO) $(FMT) -i


racket-lint:
	find $(RKTDIR) -type f -name '*.rkt' -print0 | xargs -0 -n1 $(RACO) $(REVIEW)

lint: racket-lint

racket-test:
	$(RACO) $(TEST) $(RKTDIR)

test: racket-test

.PHONY: test lint racket-lint racket-format racket-format-fix racket-test