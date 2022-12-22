#lang br
(require "lexer.rkt" brag/support rackunit)


(define (lex str)
  (apply-port-proc sdf-lexer str))

(define (test-data-keyword keyword)
  (list
    (lex keyword)
    (list
      (srcloc-token (token keyword keyword)
                    (srcloc 'string 1 0 1 (string-length keyword))))))

(define (test-data-constant constant)
  (list
    (lex constant)
    (list
      (srcloc-token (token 'CONSTANT (string->number constant))
                    (srcloc 'string 1 0 1 (string-length constant))))))

(define (test-data-identifier identifier)
  (list
    (lex identifier)
    (list
      (srcloc-token (token 'IDENTIFIER identifier)
                    (srcloc 'string 1 0 1 (string-length identifier))))))


(check-equal? (lex "") empty)

(check-equal?
 (lex " ")
 (list (srcloc-token (token " " #:skip? #t)
                     (srcloc 'string 1 0 1 1))))

(check-equal?
 (lex "// keyword module ignored\n")
 (list (srcloc-token (token "// keyword module ignored\n" #:skip? #t)
                     (srcloc 'string 1 0 1 26))))

(define keywords
  '("module" "endmodule" "input" "output"
    "(" ")" ";" "," "+" "-" "/" "*" "="))

(for
  ([keyword keywords])
  (apply
    check-equal?
    (test-data-keyword keyword)))

(define constants
  '("0" "1" "2" "10" "152" "12.53" "0.2556" "-0" "-1" "-83.54"))

(for
  ([constant constants])
  (apply
    check-equal?
    (test-data-constant constant)))

(define identifiers
  '("a" "b" "_hello" "_1" "_aasdfd4564654" "threads2"))

(for
  ([identifier identifiers])
  (apply
    check-equal?
    (test-data-identifier identifier)))

(check-exn exn:fail:read? (lambda () (lex ".3")))
(check-exn exn:fail:read? (lambda () (lex ".-3")))
(check-exn exn:fail:read? (lambda () (lex "._a1")))
(check-exn exn:fail:read? (lambda () (lex "_a1.8")))
