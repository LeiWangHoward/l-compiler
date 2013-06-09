#lang plai
(require "L5-parse-compile.rkt")
(require data/queue)
;from L5 -> L4, compile e
(define (compile-L5 exp)
  (if (empty? exp)
      '(())
      (let ([L4_main (L5-compile (L5-parse exp))])
        (cons L4_main (queue->list func-que)))))

;(define filename (command-line #:args (filename) filename))
;;benchmarks
;(define filename "../../322-interps/tests/19/5-test/12.L5")
;(define filename "../../322-interps/tests/29/5-test/11.L5")
;(define filename "../../322-interps/tests/robby/5-test/34.L5");40
;(define filename "../../../322-interps/tests/sad-rule-hand/5-test/32.L5")
;(define filename "../Lc-fest/40/40.L5")
;(define L5_exp (call-with-input-file filename read))
;(displayln (compile-L5 L5_exp))