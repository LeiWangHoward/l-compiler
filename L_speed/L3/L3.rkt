#lang plai
(require "tools.rkt")
(require "L3-compile.rkt")
;compile p of L3
(define (compile-L3-p L3_p)
  (match L3_p
    [(list (? label? v) (list args ...) e)
     (if (> (length args) 0)
         `(,v
           ,@(reg-assign args)
           ,@(compile-e e))
         (cons v
               (compile-e e)))]
    [_ (compile-e L3_p)]))

;;from L3-> L2 compile L3
(define (compile-L3 L3-exp)
   (cons `((call :_main_func));;problem!!
         (for/list ([L3_func (in-list L3-exp)]
                    [x (range (length L3-exp))])
           (let* ([L3_func (L2-key-replace L3_func)]
                  [L2_sub (compile-L3-p L3_func)])
             (if (equal? x 0) ;main function
                 (cons ':_main_func L2_sub)
                 L2_sub)))))