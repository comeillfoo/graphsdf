#lang racket

(require racket/cmdline)

(struct node (id value [fired #:mutable]) #:prefab)
(struct queue (id in out capacity [tokens #:mutable]) #:prefab)

(define/contract (node->string n)
                 (node? . -> . string?)
                 (string-append "g"
                                (number->string (node-id n))
                                " [label=\""
                                (let ([value (node-value n)])
                                  (match value
                                    [(? number?) (number->string value)]
                                    [(? string?) value]))
                                "\"]"))

(define (string-append-with-newline str ...)
  (string-append str "\n" ...))

(define/contract (queue->string q)
                 (queue? . -> . string?)
                 (string-append "g"
                                (number->string (queue-in q))
                                " -> g"
                                (number->string (queue-out q))
                                " [label=\""
                                (number->string (queue-capacity q))
                                "\"]"))

(define/contract (build-graph nodes queues)
                 (-> (listof node?) (listof queue?) string?)
                 (string-append "digraph G {\n"
                                (foldr string-append-with-newline "" (map node->string nodes))
                                (foldr string-append-with-newline "" (map queue->string queues))
                                "}"))

(define/contract (queue-in? n q) (-> node? queue? boolean?) (= (queue-out q) (node-id n)))

(define/contract (queue-out? n q) (-> node? queue? boolean?) (= (queue-in q) (node-id n)))

(define/contract (queue-none? n q)
                 (-> node? queue? boolean?)
                 (not (or (queue-in? n q) (queue-out? n q))))

(define/contract
 (fires? queues n)
 ((listof queue?) node? . -> . boolean?)
 (let ([output (filter (curry queue-out? n) queues)] [input (filter (curry queue-in? n) queues)])
   (and (andmap (lambda (q) (= (length (queue-tokens q)) (queue-capacity q))) input)
        (andmap (lambda (q) (< (length (queue-tokens q)) (queue-capacity q))) output)
        (not (node-fired n)))))

(define (fire n [inputs null])
  (list (let ([value (node-value n)])
          (match value
            [(? string?)
             (begin
               (printf "Enter ~a value: " value)
               (string->number (read-line)))]
            [(? number?) value]
            [(? procedure?) (apply value inputs)]))))

(define is-graph (make-parameter #f))

(define firings (make-parameter 1000))

(define verbose (make-parameter #f))

(define sdfir-file
  (command-line #:program "sir"
                #:once-each [("-g" "--graph") "Prints only graph in .dot format" (is-graph #t)]
                [("-f" "--firings")
                 raw-firings
                 "Upper limit for the number of firings, default 1000"
                 (firings (string->number raw-firings))]
                [("-v" "--verbose") "Enables verbose output" (verbose #t)]
                #:args (filepath) ; expect one command-line argument: <filepath>
                ; return the argument as a filepath to compile
                filepath))

(define in (open-input-file sdfir-file #:mode 'text))

(define nodes (read in))
(define queues (read in))

(close-input-port in)

(for* ([q0 queues] [q1 queues])
  (and
   (= (queue-in q0) (queue-out q1))
   (= (queue-out q0) (queue-in q1))
   (error 'queues "found simple loop between queue[~a] and queue[~a]" (queue-id q0) (queue-id q1))))

(define procedures (hash "+" + "-" - "*" * "/" / "sqrt" sqrt))

(if (is-graph)
    (displayln (build-graph nodes queues))
    (void
     (let ([nodes (map (lambda (n)
                         (node (node-id n)
                               (hash-ref procedures (node-value n) (node-value n))
                               (node-fired n)))
                       nodes)])
       (for/or ([turn (in-range (firings))])
         (let* ([fireable-nodes (filter (curry fires? queues) nodes)]
                [finish? (empty? fireable-nodes)])
           (unless finish?
             (let* ([fire-node (first fireable-nodes)]
                    [queues-in (filter (curry queue-in? fire-node) queues)]
                    [queues-out (filter (curry queue-out? fire-node) queues)]
                    [queues-rest (filter (curry queue-none? fire-node) queues)]
                    [inputs (map (lambda (q) (first (queue-tokens q))) queues-in)]
                    [token (fire fire-node inputs)])
               (if (empty? queues-out)
                   (println (first token))
                   ;; update output buffers
                   (set! queues-out
                         (map (lambda (q)
                                (set-queue-tokens! q (append token (queue-tokens q)))
                                q)
                              queues-out)))
               ;; if there were inputs then update corresponding input buffers
               (unless (empty? inputs)
                 (set! queues-in
                       (map (lambda (q)
                              (set-queue-tokens! q (list-tail (queue-tokens q) 1))
                              q)
                            queues-in)))
               (set!
                queues
                (sort (append queues-in queues-out queues-rest) < #:key (lambda (q) (queue-id q))))
               ;; mark val nodes as fired
               (when (string? (node-value fire-node))
                 (set-node-fired! fire-node #t)
                 (set! nodes (list-set nodes (node-id fire-node) fire-node))))
             (when (verbose)
               (printf "--- stage[~a/~a] ---~n" turn (firings))
               (printf "queues:~n~a~n"
                       (string-join (map (lambda (q)
                                           (format "- [~a:~a->~a]: [~a]"
                                                   (queue-id q)
                                                   (queue-in q)
                                                   (queue-out q)
                                                   (string-join (map (lambda (token)
                                                                       (number->string token))
                                                                     (queue-tokens q))
                                                                ", ")))
                                         queues)
                                    "\n"))
               (printf "nodes:~n~a~n"
                       (string-join
                        (map (lambda (n)
                               (let ([value (node-value n)])
                                 (format "- [~a]: ~a"
                                         (node-id n)
                                         (if (procedure? value)
                                             (string-append "(" (hash-ref procedures value) ")")
                                             value))))
                             nodes)
                        "\n"))))
           finish?)))))
