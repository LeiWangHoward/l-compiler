#lang plai
(require "L5-parse-compile.rkt")
(require data/queue)
;(define filename (command-line #:args (filename) filename))
(define filename "../322-interps/tests/29/5-test/11.L5")
;(define filename "../../tests/sad-rule-hand/5-test/35.L5")
(define L5-exp (call-with-input-file filename read))
;from L5 -> L4, compile e
(define (compile exp)
  (if (empty? exp)
      (displayln)
      (let ([L4_main (L5-compile (L5-parse exp))])
        (displayln
         (cons L4_main (queue->list func-que))))))

(compile L5-exp)