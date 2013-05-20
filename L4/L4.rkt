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
    [`(,(? label? v) (,(? var? args) ...) ,e)
     (if (> (length args) 0)
         (let* ([new_args (new-arg-name args)];argument replacement
                [new_e (var-replace e args new_args)])
           `(,v ,new_args  ,(norm new_e)))
         `(,v () ,(norm e)))]
    [_ (norm L4_p)]))

(define (main exp)
  (displayln
   (for/list 
       ([sub_exp (in-list exp)])
     (compile-p sub_exp))));compile p

(main L4-exp)