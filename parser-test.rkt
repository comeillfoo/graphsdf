#lang br
(require "parser.rkt" "tokenizer.rkt" brag/support)

(define program #<<HERE
a = 2.5 * c
b = a / -0.8

module transfer(input in1, input in2, output out);
  out = in1 * in2
endmodule

d = a - b

transfer(d, c, e)
f = e + d

HERE
)

(parse-to-datum
  (apply-tokenizer
    make-tokenizer
    program))
