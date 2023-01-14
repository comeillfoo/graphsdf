#lang br/quicklang

(require racket/contract)

(struct node (id value [fired #:mutable]) #:prefab)
(struct queue (id in out capacity [tokens #:mutable]) #:prefab)

(define-macro (gsdf-module-begin PARSE-TREE) #'(#%module-begin PARSE-TREE))
(provide (rename-out [gsdf-module-begin #%module-begin]))

(define-macro (gsdf-program STATEMENTS ...)
              #'(let-values ([(nodes queues nodes-count queues-count)
                              (fold-program (list STATEMENTS ...))])
                  (writeln (sort (map cdr (hash->list nodes)) < #:key node-id))
                  (writeln queues)))
(provide gsdf-program)

(define/contract (fold-program statements)
                 ((listof procedure?) . -> . (values hash? (listof queue?) number? number?))
                 (for/fold ([nodes (hash)] [queues null] [next-node-id 0] [next-queue-id 0])
                           ([stmt (in-list statements)])
                   (stmt nodes queues next-node-id next-queue-id)))

(define-macro (statement ASSIGNMENT)
              #'(lambda (nodes queues next-node-id next-queue-id)
                  (ASSIGNMENT nodes queues next-node-id next-queue-id)))
(provide statement)

(define-macro (next-object! ID)
              #'(begin0 ID
                  (set! ID (add1 ID))))

(define-macro
 (assignment IDENTIFIER "=" EXPRESSION)
 #'(lambda (nodes queues next-node-id next-queue-id)
     ; test if value already assigned
     (when (hash-has-key? nodes IDENTIFIER)
       (error 'assignment "name ~a already in use~n" IDENTIFIER))
     ; actual work
     (let* ([op (car EXPRESSION)]
            [args (cdr EXPRESSION)]
            [nodes (for/fold ([updated-nodes nodes]) ([arg args])
                     (hash-set updated-nodes
                               arg
                               (hash-ref updated-nodes
                                         arg
                                         (lambda () (node (next-object! next-node-id) arg #f)))))]
            [acc-node (node (next-object! next-node-id) op #f)]
            [arg-ids (map (compose node-id (curry hash-ref nodes)) args)]
            [arg-queues (map (lambda (arg-id)
                               (queue (next-object! next-queue-id) arg-id (node-id acc-node) 1 null))
                             arg-ids)])
       (values (hash-set nodes IDENTIFIER acc-node)
               (append queues arg-queues)
               next-node-id
               next-queue-id))))
(provide assignment)

(define-macro-cases expr
                    [(expr VALUE) #'(cons + (list VALUE))]
                    [(expr OP VALUE) #'(cons OP (list VALUE))]
                    [(expr VALUE1 OP VALUE2) #'(cons OP (list VALUE1 VALUE2))])
(provide expr)

(define-macro (ident-or-const VALUE) #'VALUE)
(provide ident-or-const)

(define (unary-op op)
  op)
(provide unary-op)

(define (binary-op op)
  op)
(provide binary-op)
