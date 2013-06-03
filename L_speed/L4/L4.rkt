#lang plai
(require data/queue)
(require "tools.rkt")
(require "L4-type.rkt")
(require "L4-algorithm.rkt")

;(define filename (command-line #:args (filename) filename))
;(define exp (call-with-input-file filename read))
;from L4 -> L3, compile p
(define (compile-p L4_p)
  (match L4_p
    [`(,(? label? v) (,(? var? args) ...) ,e)
     `(,v ,args  ,(norm e))]
    [_ (norm L4_p)]))

(define (compile-L4 exp)
  (for/list 
      ([sub_exp (in-list exp)])
    (compile-p sub_exp)))

;(displayln (compile-L4 exp))