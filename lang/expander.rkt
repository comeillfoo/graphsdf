#lang racket
#|review: ignore|#

(require (for-syntax syntax/parse)
         racket/contract
         syntax/parse/define)

(struct node (id value [fired #:mutable]) #:prefab)
(struct queue (id in out capacity [tokens #:mutable]) #:prefab)

(define/contract (hash->sorted-list hash extractor)
                 (hash? (any/c . -> . any/c) . -> . list?)
                 (sort (map cdr (hash->list hash)) < #:key extractor))

(define/contract (number->symbol n) (number? . -> . symbol?) (string->symbol (number->string n)))

(define-syntax (next! stx)
  (syntax-parse stx
    [(_ ARG)
     (unless (identifier? #'ARG)
       (raise-syntax-error #f "must be identifier" stx #'ARG))
     #'(begin0 ARG
         (set! ARG (add1 ARG)))]))

(define (imm num)
  (lambda (nodes queues n-id q-id key)
    (values (if (hash-has-key? nodes (number->symbol num))
                nodes
                (hash-set nodes (number->symbol num) (node (next! n-id) num #f)))
            queues
            n-id
            q-id
            (list (number->symbol num)))))

(define-syntax-rule (datum . literal) (imm 'literal))

(define (var id)
  (lambda (nodes queues n-id q-id key)
    (values (if (hash-has-key? nodes id)
                nodes
                (hash-set nodes id (node (next! n-id) (symbol->string id) #f)))
            queues
            n-id
            q-id
            (list id))))

(define-syntax-rule (top . ID) (var 'ID))

(define (eval op . subexprs)
  (lambda (nodes queues n-id q-id key)
    ; apply subfunctions
    (let* ([args (for/fold ([args null]) ([subexpr subexprs])
                   (set!-values (nodes queues n-id q-id key) (subexpr nodes queues n-id q-id key))
                   (append args key))]
           [args-ids (map (compose node-id (curry hash-ref nodes)) args)]
           [acc-id (next! n-id)]
           [acc-node (node acc-id op #f)]
           [acc-key (string->symbol (string-append op ":" (number->string acc-id)))])
      (values (hash-set nodes acc-key acc-node)
              (append queues (map (lambda (id) (queue (next! q-id) id acc-id 1 null)) args-ids))
              n-id
              q-id
              (list acc-key)))))

(define (add a b)
  (eval "+" a b))

(define (sub a b)
  (eval "-" a b))

(define (mul a b)
  (eval "*" a b))

(define (div a b)
  (eval "/" a b))

(define (-sqrt a)
  (eval "sqrt" a))

(define-syntax-rule (module-begin exprs ...)
  (#%module-begin
   (let-values ([(nodes queues)
                 (for/fold ([ns (hash)] [qs null] [n-id 0] [q-id 0] [_ null] #:result (values ns qs))
                           ([expr (list exprs ...)])
                   (expr ns qs n-id q-id null))])
     (writeln (hash->sorted-list nodes node-id))
     (writeln queues))))

(provide (except-out (all-from-out racket) #%module-begin #%top #%datum + - * / sqrt)
         (rename-out [module-begin #%module-begin]
                     [top #%top]
                     [datum #%datum]
                     [add +]
                     [sub -]
                     [mul *]
                     [div /]
                     [-sqrt sqrt]))
