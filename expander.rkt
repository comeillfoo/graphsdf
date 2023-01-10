#lang br/quicklang

(require racket/contract racket/generator)

(define (undefined-op op)
  (error 'op "operation ~a unknown~n" op))

(struct queue (id in out) #:transparent)
(struct node (id value) #:transparent)

(define next-node-id
  (sequence->generator (in-naturals)))

(define next-queue-id
  (sequence->generator (in-naturals)))


(define-macro (gsdf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [gsdf-module-begin #%module-begin]))


(define-macro (gsdf-program STATEMENTS ...)
  #'(let-values
    ([(nodes queues) (fold-program (list STATEMENTS ...))])
    (void (build-ir nodes queues))))
(provide gsdf-program)


(define operations
  (hash
    + "add"
    - "sub"
    * "mul"
    / "div"
    sqrt "sqrt"))


(define (build-ir nodes queues)
  (let
    ([nodes (sort (hash-values nodes) < #:key node-id)])
    (for
      ([n nodes])
      (for
        ([q queues])
        (let
          ([id (node-id n)]
           [in (queue-in q)]
           [out (queue-out q)])
          (printf
            (~a
              #:align 'right
              #:min-width 3
              (cond
                [(= in  id)   1]
                [(= out id)  -1]
                [else         0])))))
      (newline))
    (newline)
    (for
      ([n nodes])
      (printf
        (match (node-value n)
          [(? procedure? op) (format "~a~n" (hash-ref operations op (lambda (op) (undefined-op op))))]
          [(? number? constant) (format "imm ~a~n" constant)]
          [(? string? identifier) (format "val ~a~n" identifier)])))))

(define/contract (fold-program statements)
  ((listof procedure?) . -> . (values hash? (listof queue?)))
  (for/fold
    ([nodes (hash)]
     [queues null])
    ([stmt (in-list statements)])
    (stmt nodes queues)))


(define-macro (statement ASSIGNMENT)
  #'(lambda (nodes queues)
      (ASSIGNMENT nodes queues)))
(provide statement)


(define-macro (assignment IDENTIFIER "=" EXPRESSION)
  #'(lambda
      (nodes queues)
      ; test if value already assigned
      (when (hash-has-key? nodes IDENTIFIER)
        (error 'assignment "name ~a already in use~n" IDENTIFIER))
      ; actual work
      (let*
        ([op   (car EXPRESSION)]
         [args (cdr EXPRESSION)]
         [nodes
          (for/fold
            ([updated-nodes nodes])
            ([arg args])
            (hash-set updated-nodes arg (hash-ref updated-nodes arg (node (next-node-id) arg))))]
         [acc-node (node (next-node-id) op)]
         [arg-ids (map (compose node-id (curry hash-ref nodes)) args)]
         [arg-queues
          (map
            (lambda (arg-id)
              (queue (next-queue-id) arg-id (node-id acc-node)))
              arg-ids)])
        (values
          (hash-set
            nodes
            IDENTIFIER
            acc-node)
          (append queues arg-queues)))))
(provide assignment)


(define-macro-cases expr
  [(expr VALUE) #'(cons + (list VALUE))]
  [(expr OP VALUE) #'(cons OP (list VALUE))]
  [(expr VALUE1 OP VALUE2) #'(cons OP (list VALUE1 VALUE2))])
(provide expr)

(define-macro (ident-or-const VALUE)
  #'VALUE)
(provide ident-or-const)


(define procedures
  (hash
    "-" -
    "+" +
    "*" *
    "/" /
    "sqrt" sqrt))


(define (unary-op op)
  (hash-ref procedures op (lambda () (undefined-op op))))
(provide unary-op)


(define (binary-op op)
  (hash-ref procedures op (lambda () (undefined-op op))))
(provide binary-op)
