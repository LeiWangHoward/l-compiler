#lang plai
(require "L5/L5.rkt")
(require "L4/L4.rkt")
(require "L3/L3.rkt")
(require "L2/L2.rkt")
(require "L1/L1.rkt")
(define filename (command-line #:args (filename) filename))
;(define filename "../322-interps/tests/robby/5-test/15.L5");10
(define L5_exp (call-with-input-file filename read))

;(displayln (compile-L5 L5_exp))
;(compile-L5 L5_exp)
;(compile-L4 (compile-L5 L5_exp))
;(compile-L3 (compile-L4 (compile-L5 L5_exp)))
;(compile-L2 (compile-L3 (compile-L4 (compile-L5 L5_exp))))
;(compile-L1 (compile-L2 (compile-L3 (compile-L4 (compile-L5 L5_exp)))))
(let* ([L4_exp (compile-L5 L5_exp)]
       [L3_exp (compile-L4 L4_exp)]
       [L2_exp (compile-L3 L3_exp)]
       [L1_exp (compile-L2 L2_exp)])
  (compile-L1 L1_exp))
