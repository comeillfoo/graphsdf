#lang racket

(module+ main
  (require math/matrix
           racket/generator
           brag/support)

  (match-let ([(list m n) (map (lambda (raw-size) (string->number raw-size))
                               (string-split (read-line)))])

    (for/matrix m
                n
                #:fill 0
                ([token
                  (in-generator
                   (for ([row (in-stream (for/stream ([count (in-range m)]) (read-line)))])
                     (for ([tokens (map (lambda (raw-element) (string->number raw-element))
                                        (string-split row))])
                       (yield tokens))))])
                token)

    (for/list ([_ (in-range n)])
      (let ([node (read-line)])
        (match node
          ["add" (token 'add +)]
          ["sub" (token 'sub -)]
          ["mul" (token 'mul *)]
          ["div" (token 'div /)]
          [(pregexp #px"imm\\s{1,}(-{,1}\\d{1,}(\\.\\d{1,}){,1})" (list _ raw-imm _))
           (token 'imm (string->number raw-imm))]
          [(pregexp #px"val\\s{1,}([[:alpha:]_]{1}\\w{0,})" (list _ raw-val))
           (token 'val (string->symbol raw-val))])))))
