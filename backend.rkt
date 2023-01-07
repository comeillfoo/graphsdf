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

(struct node (id type value [fired #:mutable]) #:transparent)

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


(struct queue (id in out capacity [tokens #:mutable]) #:transparent)

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


(define/contract (queue-in? node queue)
  (-> node? queue? boolean?)
    (= (queue-out queue) (node-id node)))


(define/contract (queue-out? node queue)
  (-> node? queue? boolean?)
    (= (queue-in queue) (node-id node)))


(define/contract (queue-none? node queue)
  (-> node? queue? boolean?)
    (not
      (or
        (queue-in? node queue)
        (queue-out? node queue))))


(define/contract (fires? queues node)
  ((listof queue?) node? . -> . boolean?)
  (let
    ([output (filter (curry queue-out? node) queues)]
     [input (filter (curry queue-in? node) queues)])
    (and
      (andmap
        (lambda (q)
          (=
            (length (queue-tokens q))
            (queue-capacity q)))
        input)
      (andmap
        (lambda (q)
          (<
            (length (queue-tokens q))
            (queue-capacity q)))
        output)
      (not (node-fired node)))))


(define (fire node [inputs null])
  (list
    (let
      ([value (node-value node)])
      (match value
        [(? string?) (begin (printf "Enter ~a value: " value) (string->number (read-line)))]
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
    ([columns (vector-length (vector-ref topology-matrix 0))]
     [queues
      (for/vector
        ([index (range columns)])
        (vector-map
          (lambda
            (vec)
            (vector-ref vec index))
          topology-matrix))])
    (for/list
      ([q queues]
       [id (in-naturals)])
      (let*
        ([input-node (index-where (vector->list q) positive-integer?)]
         [output-node (index-where (vector->list q) negative-integer?)])
        (queue id input-node output-node (vector-ref q input-node) null)))))

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
    (make-parameter #f))

  (define firings
    (make-parameter 1000))

  (define verbose
    (make-parameter #f))

  (define sdfir-file
    (command-line
    #:program "backend"
    #:once-each
    [("-g" "--graph") "Prints only graph in .dot format" (is-graph #t)]
    [("-f" "--firings") raw-firings "Upper limit for the number of firings, default 1000" (firings (string->number raw-firings))]
    [("-v" "--verbose") "Enables verbose output" (verbose #t)]
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
            ["add" +]
            ["sub" -]
            ["mul" *]
            ["div" /]
            [(pregexp #px"imm\\s{1,}(-{,1}\\d{1,}(\\.\\d{1,}){,1})" (list _ raw-imm _))
            (string->number raw-imm)]
            [(pregexp #px"val\\s{1,}([[:alpha:]_]{1}\\w{0,})" (list _ raw-val))
            raw-val])
          #f))))

  (close-input-port in)

  (define queues (matrix->queues/list topology-matrix))

  (if (is-graph)
    (displayln
      (build-graph
        nodes
        queues))
    (begin
      (for/or
        ([turn (in-range (firings))])
        (let*
          ([fireable-nodes (filter (curry fires? queues) nodes)]
          [finish? (empty? fireable-nodes)])
          (unless
            finish?
            (let*
              ([fire-node (first fireable-nodes)]
              [queues-in (filter (curry queue-in? fire-node) queues)]
              [queues-out (filter (curry queue-out? fire-node) queues)]
              [queues-rest (filter (curry queue-none? fire-node) queues)]
              [inputs (map (lambda (q) (first (queue-tokens q))) queues-in)]
              [token (fire fire-node inputs)])
              (if (empty? queues-out)
                (println (first token))
                ;; update output buffers
                (set!
                  queues-out
                  (map
                    (lambda (q) (set-queue-tokens! q (append token (queue-tokens q))) q)
                    queues-out)))
              ;; if there were inputs then update corresponding input buffers
              (unless
                (empty? inputs)
                (set!
                  queues-in
                  (map
                    (lambda (q) (set-queue-tokens! q (list-tail (queue-tokens q) 1)) q)
                    queues-in)))
              (set!
                queues
                (sort
                  (append
                    queues-in
                    queues-out
                    queues-rest)
                  <
                  #:key (lambda (q) (queue-id q))))
              ;; mark val nodes as fired
              (when (string? (node-value fire-node))
                (set-node-fired! fire-node #t)
                (set! nodes (list-set nodes (node-id fire-node) fire-node))))
            (when (verbose)
              (printf "--- stage[~a/~a] ---~n" turn (firings))
              (printf "queues:~n~a~n"
                (string-join
                  (map
                    (lambda (q)
                      (format
                        "- [~a:~a->~a]: [~a]"
                        (queue-id q)
                        (queue-in q)
                        (queue-out q)
                        (string-join
                          (map (lambda (token) (number->string token)) (queue-tokens q))
                          ", ")))
                    queues)
                  "\n"))
              (printf "nodes:~n~a~n"
                (string-join
                  (map
                    (lambda
                      (n)
                      (let ([value (node-value n)])
                        (format
                          "- [~a]: ~a"
                          (node-id n)
                          (if (procedure? value)
                            (string-append
                              "("
                              (hash-ref procedures value)
                              ")")
                            value))))
                    nodes)
                  "\n"))))
          finish?))
        (void))))
