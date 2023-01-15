#lang racket

(module structs racket
  (provide (all-defined-out))
  (struct node (id value [fired #:mutable]) #:prefab)
  (struct queue (id in out capacity [tokens #:mutable]) #:prefab)
  (void (queue 0 0 0 0 null)))

(module graphic racket
  (provide (all-defined-out))
  (require (submod ".." structs))
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
                                  "}")))

(module executor racket
  (require (submod ".." structs))
  (provide (all-defined-out))
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

  (define/contract (push-queue tokens q)
                   ((listof number?) queue? . -> . queue?)
                   (set-queue-tokens! q (append tokens (queue-tokens q)))
                   q)

  (define/contract (pop-queue q)
                   (queue? . -> . queue?)
                   (set-queue-tokens! q (list-tail (queue-tokens q) 1))
                   q)

  (define/contract
   (run-step nodes queues)
   ((listof node?) (listof queue?) . -> . (values (listof node?) (listof queue?) boolean?))
   (let* ([fireable-nodes (filter (curry fires? queues) nodes)] [finish? (empty? fireable-nodes)])
     (cond
       [finish? (values nodes queues finish?)]
       [else
        (let* ([fire-node (first fireable-nodes)]
               [queues-in (filter (curry queue-in? fire-node) queues)]
               [queues-out (filter (curry queue-out? fire-node) queues)]
               [queues-rest (filter (curry queue-none? fire-node) queues)]
               [inputs (map (compose1 first queue-tokens) queues-in)]
               [token (fire fire-node inputs)])
          ;; update output queues or print result
          (cond
            [(empty? queues-out) (println (first token))]
            [else (set! queues-out (map (curry push-queue token) queues-out))])
          ;; update input queues if there were
          (unless (empty? inputs)
            (set! queues-in (map pop-queue queues-in)))
          ;; update the whole queues list
          (set! queues (sort (append queues-in queues-out queues-rest) < #:key queue-id))
          ;; mark val node as fired
          (when (string? (node-value fire-node))
            (set-node-fired! fire-node #t)
            (set! nodes (list-set nodes (node-id fire-node) fire-node)))
          (values nodes queues finish?))]))))

(module verbosity racket
  (provide (except-out (all-defined-out) procedures))
  (require (submod ".." structs))
  (define procedures (hash + "+" - "-" / "/" * "*" sqrt "sqrt"))

  (define/contract (print-stage current-stage limit)
                   (exact-integer? exact-integer? . -> . void?)
                   (printf "--- stage[~a/~a] ---~n" current-stage limit))

  (define/contract (format-tokens tokens)
                   ((listof number?) . -> . string?)
                   (string-join (map (lambda (token) (number->string token)) tokens) ", "))

  (define/contract (format-queue q)
                   (queue? . -> . string?)
                   (format "- [~a:~a->~a]: [~a]"
                           (queue-id q)
                           (queue-in q)
                           (queue-out q)
                           (format-tokens (queue-tokens q))))

  (define/contract (print-queues queues)
                   ((listof queue?) . -> . void?)
                   (printf "queues:~n~a~n" (string-join (map format-queue queues) "\n")))

  (define/contract (format-node n)
                   (node? . -> . string?)
                   (let ([value (node-value n)])
                     (format "- [~a]: ~a"
                             (node-id n)
                             (if (procedure? value)
                                 (string-append "(" (hash-ref procedures value "undefined") ")")
                                 value))))

  (define/contract (print-nodes nodes)
                   ((listof node?) . -> . void?)
                   (printf "nodes:~n~a~n" (string-join (map format-node nodes) "\n"))))

;;; require dependencies
(require racket/cmdline
         'structs
         'graphic
         'executor
         'verbosity)

;;; command line parameters
(define is-graph (make-parameter #f))
(define firings (make-parameter 1000))
(define verbose (make-parameter #f))
(define in
  (open-input-file
   (command-line #:program "sir"
                 #:once-each [("-g" "--graph") "Prints only graph in .dot format" (is-graph #t)]
                 [("-f" "--firings")
                  raw-firings
                  "Upper limit for the number of firings, default 1000"
                  (firings (string->number raw-firings))]
                 [("-v" "--verbose") "Enables verbose output" (verbose #t)]
                 #:args (filepath) ; expect one command-line argument: <filepath>
                 ; return the argument as a filepath to compile
                 filepath)
   #:mode 'text))

;;; initial state parameters
(define init-nodes (read in))
(define init-queues (read in))

(close-input-port in)

;;; look for the simple loops
(for* ([q0 init-queues] [q1 init-queues])
  (and
   (= (queue-in q0) (queue-out q1))
   (= (queue-out q0) (queue-in q1))
   (error 'queues "found simple loop between queue[~a] and queue[~a]" (queue-id q0) (queue-id q1))))

(define string->procedures (hash "+" + "-" - "*" * "/" / "sqrt" sqrt))

;;; main
(cond
  [(is-graph) (displayln (build-graph init-nodes init-queues))]
  [else
   (let ([init-nodes (map (lambda (n)
                            (node (node-id n)
                                  (hash-ref string->procedures (node-value n) (node-value n))
                                  (node-fired n)))
                          init-nodes)])
     (for/fold ([nodes init-nodes] [queues init-queues] [finish? #f] #:result (void))
               ([turn (in-range (firings))])
       #:break finish?
       (when (verbose)
         (print-stage turn (firings))
         (print-queues queues)
         (print-nodes nodes))
       (run-step nodes queues)))])
