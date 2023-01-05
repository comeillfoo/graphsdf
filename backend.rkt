#lang racket

(module+ test
  (require rackunit))

(module+ main
  (require racket/cmdline))

(define/contract (node->type node-number topology-matrix)
  (-> exact-integer? (vectorof (vectorof exact-integer?)) symbol?)
  (let*
    ([row (vector-ref topology-matrix node-number)]
     [produces (> (vector-count positive-integer? row) 0)]
     [consumes (> (vector-count negative-integer? row) 0)])
    (match
      (list produces consumes)
      [(list #t #t) 'inout]
      [(list #t #f) 'in]
      [(list #f #t) 'out]
      [_ 'undef])))

(struct node (id type value) #:transparent)

(define procedures
  (hash
    + "+"
    - "-"
    * "*"
    / "/"))

(define/contract (node->string node)
  (node? . -> . string?)
  (string-append
    "g"
    (number->string (node-id node))
    " [label=\""
    (let
      ([value (node-value node)])
      (match value
        [(? procedure?) (hash-ref procedures value "undefined")]
        [(? number?) (number->string value)]
        [(? string?) value]))
    "\"]"))


(define (string-append-with-newline str ...)
  (string-append str "\n" ...))


(struct queue (in out capacity tokens) #:transparent)

(define/contract (queue->string queue)
  (queue? . -> . string?)
  (string-append
    "g"
    (number->string (queue-in queue))
    " -> g"
    (number->string (queue-out queue))
    " [label=\""
    (number->string (queue-capacity queue))
    "\"]"))


(define/contract (fires? node queues)
  (node? (listof queue?) . -> . boolean?)
  (let*
    ([id (node-id node)]
     [input (filter (lambda (q) (= (queue-in q) id)) queues)]
     [output (filter (lambda (q) (= (queue-out q) id)) queues)])
    (and
      (map
        (lambda (q)
          (=
            (length (queue-tokens q))
            (queue-capacity q)))
        input)
      (map
        (lambda (q)
          (<
            (length (queue-tokens q))
            (queue-capacity q)))
        output))))


(define (fire node [inputs null])
  (list
    (let
      ([value (node-value node)])
      (match value
        [(? string?) (begin (printf "Enter ~a value: " ) (string->number (read-line)))]
        [(? number?) value]
        [(? procedure?) (apply value inputs)]))))


(define/contract (build-graph nodes queues)
  (-> (listof node?) (listof queue?) string?)
  (string-append
    "digraph G {\n"
    (foldr
      string-append-with-newline
      ""
      (map
        node->string
        nodes))
    (foldr
      string-append-with-newline
      ""
      (map
        queue->string
        queues))
    "}"))


(define/contract (matrix->queues/list topology-matrix)
  ((vectorof (vectorof exact-integer?)) . -> . (listof queue?))
  (let*
    ([queues (vector-length (vector-ref topology-matrix 0))]
     [raw-queues
      (for/vector
        ([index (range queues)])
        (vector-map
          (lambda
            (vec)
            (vector-ref vec index))
          topology-matrix))])
    (for/list
      ([raw-queue raw-queues])
      (let*
        ([input-node (index-where (vector->list raw-queue) positive-integer?)]
         [output-node (index-where (vector->list raw-queue) negative-integer?)])
        (queue input-node output-node (vector-ref raw-queue input-node) null)))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.
  (check-equal? (fire (node 0 'out 8)) '(8))
  (check-equal? (fire (node 0 'out -) '(8 5)) '(3)))


(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
  (define is-graph
    (make-parameter
      #f
      boolean?))

  (define sdfir-file
    (command-line
    #:program "backend"
    #:once-each
    [("-g" "--graph") "Builds only graph in .dot format" (is-graph #t)]
    #:args (filepath) ; expect one command-line argument: <filepath>
    ; return the argument as a filepath to compile
    filepath))

  (define in (open-input-file sdfir-file #:mode 'text))

  (define topology-matrix
    (for/vector
      ([row (in-lines in)])
      #:break (not (non-empty-string? row))
      (for/vector
        ([column (string-split row)])
        (string->number column))))

  (define nodes
    (for/list
      ([idx (in-range (vector-length topology-matrix))])
      (let
        ([raw-node (read-line in)]
         [type (node->type idx topology-matrix)])

        (node
          idx
          type
          (match raw-node
            ["add" '+]
            ["sub" '-]
            ["mul" '*]
            ["div" '/]
            [(pregexp #px"imm\\s{1,}(-{,1}\\d{1,}(\\.\\d{1,}){,1})" (list _ raw-imm _))
            (string->number raw-imm)]
            [(pregexp #px"val\\s{1,}([[:alpha:]_]{1}\\w{0,})" (list _ raw-val))
            raw-val])))))

  (close-input-port in)

  (define queues (matrix->queues/list topology-matrix))

  (if (is-graph)
    (displayln
      (build-graph
        nodes
        queues))
    (values nodes queues)))
