#lang racket

(module+ test
  (require rackunit))

;; Code here
(require racket/cmdline)

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





(define/contract (node->type node-number topology-matrix)
  (-> exact-integer? vector? symbol?)
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

(struct node (type value) #:transparent)


(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.
  (void))


(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
  (define in (open-input-file sdfir-file #:mode 'text))

  (define topology-matrix
    (for/vector
      ([row (in-lines in)])
      #:break (not (non-empty-string? row))
      (for/vector
        ([column (string-split row)])
        (string->number column))))

  (define nodes (vector-length topology-matrix))

  (define nodes-list
    (for/list
      ([idx (in-range nodes)])
      (let
        ([raw-node (read-line in)]
         [type (node->type idx topology-matrix)])

        (node
          type
          (match raw-node
            ["add" +]
            ["sub" -]
            ["mul" *]
            ["div" /]
            [(pregexp #px"imm\\s{1,}(-{,1}\\d{1,}(\\.\\d{1,}){,1})" (list _ raw-imm _))
            (string->number raw-imm)]
            [(pregexp #px"val\\s{1,}([[:alpha:]_]{1}\\w{0,})" (list _ raw-val))
            (string->symbol raw-val)])))))

  (close-input-port in)
  nodes-list)
