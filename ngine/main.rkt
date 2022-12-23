#lang racket


(module+ main
  (require math/matrix racket/generator)

  (match-let
    ([(list m n)
      (map
        (lambda (raw-size) (string->number raw-size))
        (string-split (read-line)))])

    (for/matrix
      m
      n
      #:fill 0
      ([token
        (in-generator
          (for
            ([row (in-lines)])
            (for
              ([tokens (map (lambda (raw-element) (string->number raw-element)) (string-split row))])
              (yield tokens))))])
      token)))
