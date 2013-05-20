#lang plai
(require data/queue)
(require "tools.rkt")
(require "L4-type.rkt")
(define filename (command-line #:args (filename) filename))
(define L3-exp (call-with-input-file filename read))
;from L3 -> L2, compile p
(define (compile-p L4_p)
  (match L4_p
    [(list (? label? v) (list args ...) e)
     (if (> (length args) 0)
         (append `(,v)
                 (reg-assign args)
                 (compile-e e))
         (append `(,v)
                 (compile-e e)))]
    [_ (compile-e L4_p)]))
