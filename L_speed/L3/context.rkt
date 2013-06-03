#lang plai
(require "tools.rkt")
(define-type context
  [let-ctxt (x var?)
            (b L4-e?)
            (k context?)]
  [if-ctxt (t L4-e?)
           (e L4-e?)
           (k context?)]
  [fun-ctxt (a L4-e?)
            (k context?)]
  [arg-ctxt (f val?)
            (k context?)]
  [no-ctxt])
