#lang plai
(require data/queue)
(require "tools.rkt")
(require "L4-type.rkt")
(require "L4-algorithm.rkt")
;(define filename "4-test/02.L4")
(define filename (command-line #:args (filename) filename))
(define L4-exp (call-with-input-file filename read))
;from L3 -> L2, compile p
(define (compile-p L4_p)
  (match L4_p
    [(list (? label? v) (list args ...) e)
     (if (> (length args) 0)
         `(,v ,args  ,(norm e))
         `(,v ,(norm e)))]
    [_ (norm L4_p)]))

(define (main exp)
  (displayln
   (for/list 
       ([sub_exp (in-list exp)])
     (compile-p sub_exp))))
(main L4-exp)