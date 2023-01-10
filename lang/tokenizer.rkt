#lang br
(require brag/support
         "lexer.rkt")

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token)
    (gsdf-lexer ip))
  next-token)

(provide make-tokenizer)
