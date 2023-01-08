#lang br/quicklang

(define-macro (gsdf-module-begin PARSE-TREE)
  #'(#%module-begin
     'PARSE-TREE))
(provide (rename-out [gsdf-module-begin #%module-begin]))