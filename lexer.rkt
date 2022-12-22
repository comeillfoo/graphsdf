#lang br
(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define-lex-abbrev lower-latin (char-set "abcdefghijklmnopqrstuvwxyz"))
(define-lex-abbrev upper-latin (char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define-lex-abbrev nondigits (:+ (:or lower-latin upper-latin (char-set "_"))))

(define sdf-lexer
  (lexer-srcloc
    ["\n" (token 'NEWLINE lexeme)]
    [whitespace (token lexeme #:skip? #t)]
    [(from/stop-before "//" "\n") (token lexeme #:skip? #t)]
    [(:or "module" "endmodule" "input" "output"
      "(" ")" ";" "," "+" "-" "/" "*") (token lexeme lexeme)]
    [(:seq digits (:? "." digits))
      (token 'CONSTANT (string->number lexeme))]
    [(:seq nondigits (:or nondigits digits))
      (token 'IDENTIFIER lexeme)]))

(provide sdf-lexer)
