#lang plai
(require "L5-parse-compile.rkt")
(require data/queue)
;from L5 -> L4, compile e
(define (compile-L5 exp)
  (if (empty? exp)
      '(())
      (let ([L4_main (L5-compile (L5-parse exp))])
        (cons L4_main (queue->list func-que)))))

(define filename (command-line #:args (filename) filename))
(define L5_exp (call-with-input-file filename read))
(displayln (compile-L5 L5_exp))