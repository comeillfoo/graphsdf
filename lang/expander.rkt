#lang racket

(require (for-syntax syntax/parse) syntax/parse/define racket/contract)


(struct node (id value [fired #:mutable]) #:prefab)
(struct queue (id in out capacity [tokens #:mutable]) #:prefab)

(define/contract (hash->sorted-list hash extractor)
    (hash? (any/c . -> . any/c) . -> . list?)
    (sort (map cdr (hash->list hash)) < #:key extractor))

(define/contract (number->symbol n)
    (number? . -> . symbol?)
    (string->symbol (number->string n)))

(define-syntax (next! stx)
    (syntax-parse stx
        [(_ ARG)
         (unless (identifier? #'ARG) (raise-syntax-error #f "must be identifier" stx #'ARG))
         #'(begin0 ARG (set! ARG (add1 ARG)))]))

(define nodes (hash))
(define queues null)
(define n-id 0)
(define q-id 0)


(define (imm v)
    (set!-values
        (nodes queues n-id q-id)
        (values
            (if (hash-has-key? nodes (number->symbol v))
                nodes
                (hash-set nodes (number->symbol v) (node (next! n-id) v #f)))
            queues
            n-id
            q-id))
    ; (writeln nodes)
    (number->symbol v))


(define (eval op args ...)
    (let*
        ([args-keys (list args ...)]
         [args-ids (map (compose node-id (curry hash-ref nodes)) args-keys)]
         [acc-id (next! n-id)]
         [acc-value (node acc-id op #f)]
         [acc-key (string->symbol (string-append op ":" (number->string acc-id)))])
        (set!-values
            (nodes queues n-id q-id)
            (values
                (hash-set nodes acc-key acc-value)
                (append
                    queues
                    (map
                        (lambda
                            (id)
                            (queue (next! q-id) id acc-id 1 null))
                        args-ids))
                n-id
                q-id))
        acc-key))

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


(define-syntax-rule (module-begin expr ...)
    (#%module-begin
        (void expr ...)
        (writeln (hash->sorted-list nodes node-id))
        (writeln queues)))

(provide
    (except-out
        (all-from-out racket)
        #%module-begin
        +
        -
        *
        /
        sqrt
        const)
    (rename-out
        [add +]
        [sub -]
        [mul *]
        [div /]
        [imm const]
        [-sqrt sqrt]
        [module-begin #%module-begin])
    nodes
    queues)
