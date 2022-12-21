#lang br
(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define sdf-lexer
  (lexer-srcloc
    ["\n" (token 'NEWLINE lexeme)]
    [whitespace (token lexeme #:skip? #t)]
    [(from/stop-before "//" "\n") (token lexeme #:skip? #t)]
    [(:or "module" "endmodule" "input" "output"
      "(" ")" ";" "," "+" "-" "/" "*") (token lexeme lexeme)]
    [(:or (:seq digits (:? "." digits)))
      (token 'CONSTANT (string->number lexeme))]))

(provide sdf-lexer)
