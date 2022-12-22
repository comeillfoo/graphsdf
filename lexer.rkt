#lang br
(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define-lex-abbrev lower-latin (char-set "abcdefghijklmnopqrstuvwxyz"))
(define-lex-abbrev upper-latin (char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define-lex-abbrev nondigits (:+ (:or lower-latin upper-latin "_")))

(define sdf-lexer
  (lexer-srcloc
    ["\n" (token 'NEWLINE lexeme)]
    ;; blank symbols
    [whitespace (token lexeme #:skip? #t)]
    ;; comments
    [(from/stop-before "//" "\n") (token lexeme #:skip? #t)]
    ;; keywords
    [(:or "module" "endmodule" "input" "output"
      "(" ")" ";" "," "+" "-" "/" "*") (token lexeme lexeme)]
    ;; float and integer constants
    [(:seq (:? "-") digits (:? (:seq "." digits)))
      (token 'CONSTANT (string->number lexeme))]
    ;; identifiers
    [(:seq nondigits (:? digits))
      (token 'IDENTIFIER lexeme)]))

(provide sdf-lexer)
