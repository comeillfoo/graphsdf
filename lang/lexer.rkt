#lang br
(require brag/support)

(define-lex-abbrev digit (char-set "0123456789"))
(define-lex-abbrev digits (:+ digit))

(define-lex-abbrev lower-latin (char-set "abcdefghijklmnopqrstuvwxyz"))
(define-lex-abbrev upper-latin (char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define-lex-abbrev nondigit (:or lower-latin upper-latin "_"))

(define-lex-abbrev word (:or nondigit digit))

(define gsdf-lexer
  (lexer-srcloc
   ;; blank symbols
   [whitespace (token lexeme #:skip? #t)]
   ;; comments
   [(from/to "//" "\n") (token lexeme #:skip? #t)]
   ;; keywords
   [(:or "module" "endmodule" "input" "output" "(" ")" ";" "," "+" "-" "/" "*" "=" "sqrt")
    (token lexeme lexeme)]
   ;; float and integer constants
   [(:seq (:? "-") digits (:? (:seq "." digits))) (token 'CONSTANT (string->number lexeme))]
   ;; identifiers
   [(:seq nondigit (:* word)) (token 'IDENTIFIER lexeme)]))

(provide gsdf-lexer)
