#lang br
(require "parser.rkt" "tokenizer.rkt" brag/support)

(define only-statements #<<HERE
a = 2 * c
b = a + c
d = a - b
f = e + d
HERE
)

(parse-to-datum
  (apply-tokenizer
    make-tokenizer
    only-statements))
