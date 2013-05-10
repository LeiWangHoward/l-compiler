#lang plai
(require data/queue)
;(define filename (command-line #:args (filename) filename))
;(define filename "../../322-interps/tests/34/2-test/06.L2")
(define filename "robby1.L3")
(define inst_in (call-with-input-file filename read))
;(define what_init (list 'let (list '#\[ 'a (list '- 'a 2) '#\]))); automatic [ to ( ? cooooolllll!!!!
(displayln inst_in)