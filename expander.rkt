#lang br/quicklang

(require racket/contract)

(struct queue (input-id output-id) #:transparent)
(struct node (id type value) #:transparent)
(struct gmod (name queues nodes [submods #:mutable] args) #:transparent)

(define todo (curry error "todo!"))


(define-macro (gsdf-module-begin PARSE-TREE)
  #'(#%module-begin
     'PARSE-TREE))
(provide (rename-out [gsdf-module-begin #%module-begin]))


(define-macro (gsdf-program STMT-OR-MOD ...)
  #'(begin
      (define root (fold-program (list STMT-OR-MOD ...)))
      ;; convert root module to IR
      ""))
(provide gsdf-program)


(define/contract (fold-program stmts-or-mods)
  (list? . -> . gmod?)
  (for/fold
    ([root (gmod "root" null (hash) (hash) null)])
    ([stmt-or-mod (in-list stmts-or-mods)])
    (stmt-or-mod root)))


(define-macro
  (module-definition
    "module"
    MODULE-NAME
    "("
    INPUT-OR-OUTPUT-PORT0
    INPUT-OR-OUTPUT-PORTS ...
    ")"
    ";"
    STMT0
    STMTS ...
    "endmodule")
  #'(lambda (parent)
      (let*
        ([submods (gmod-submods parent)]
         [ports   (list INPUT-OR-OUTPUT-PORT0 INPUT-OR-OUTPUT-PORTS ...)]
         [inputs  (map cdr (filter (lambda (p) (string=? (car p) "input")) ports))]
         [outputs (map cdr (filter (lambda (p) (string=? (car p) "output")) ports))]
         [submod  (gmod MODULE-NAME null (hash) (hash) ports)]
         [statements (reverse (list STMT0 STMTS ...))])
        (when
          (hash-has-key? submods MODULE-NAME)
          (error 'module-definition "module ~a is already defined~n" MODULE-NAME))
        (when
          (empty? outputs)
          (error 'module-definition "no output ports in module ~a provided~n" MODULE-NAME))
        ;; process statements
        (set! submod ((apply compose1 statements) parent submod))
        ;; add new submodule to submodules
        (set-gmod-submods! parent (hash-set submods MODULE-NAME submod))
        parent)))
(provide module-definition)


(define-macro (input-or-output-port TYPE PORT-NAME)
  #'(cons TYPE PORT-NAME))
(provide input-or-output-port)


(define-macro (statement ASSIGN-OR-MODULE-INV)
  #'(lambda (mod)
      ;; modify "mod"
      (ASSIGN-OR-MODULE-INV mod)
      mod))
(provide statement)


(define-macro (module-invocation MODULE-NAME "(" ARG0 ARGS ... ")")
  #'(lambda (mod)
      (let*
        ([submodules (gmod-submods mod)]
         [args (list ARG0 ARGS ...)])
        (if (hash-has-key? submodules MODULE-NAME)
          (let*
            ([submod (hash-ref submodules MODULE-NAME)]
             [submod-arity (length (gmod-args submod))])
            (if (= (length args) submod-arity)
              (begin
                ;; modify mod
                mod)
              (raise-arity-error (string->symbol MODULE-NAME) submod-arity ARG0 ARGS ...)))
          (error 'module-invocation "module \"~a\" is not defined~n" MODULE-NAME)))))
(provide module-invocation)


(define-macro (assignment IDENTIFIER "=" EXPRESSION)
  #'(lambda
      (queues nodes)
      (when (hash-has-key? nodes IDENTIFIER)
        (error 'assignment "name ~a already in use~n" IDENTIFIER))
      (let
        ([op (car EXPRESSION)]
         [args (cdr EXPRESSION)])
        (match op
          ['undef ]))
      (values queues nodes)))
(provide assignment)


(define-macro-cases expr
  [(expr VALUE) #'(cons 'undef (list VALUE))]
  [(expr OP VALUE) #'(cons OP (list VALUE))]
  [(expr VALUE1 OP VALUE2) #'(cons OP (list VALUE1 VALUE2))])
(provide expr)

(define-macro (ident-or-const VALUE)
  #'VALUE)
(provide ident-or-const)


(define-macro (unary-op OP)
  #'(string->symbol OP))
(provide unary-op)


(define-macro (binary-op OP)
  #'(string->symbol OP))
(provide binary-op)
