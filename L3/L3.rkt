#lang plai
(require data/queue)
(require "tools.rkt")
(require "L3-compile.rkt")
(define filename "../322-interps/tests/robby/3-test/0.L3")
;(define filename "../322-interps/tests/coal-soup-push/3-test/02.L3")
;(define filename "../322-interps/tests/edge-care-push/3-test/double.L3")
;(define filename (command-line #:args (filename) filename))
(define L3-exp (call-with-input-file filename read))
;from L3 -> L2, compile p
(define (compile-p L3_p)
  (match L3_p
    [(list (? label? v) (list args ...) e)
     (if (> (length args) 0)
         (append `(,v)
                 (reg-assign args)
                 (compile-e e))
         (append `(,v)
                 (compile-e e)))]
    [_ (compile-e L3_p)]))

;tests
#|(module+ test
  (test (compile-p '(if 5
                        (print test)
                        (print 2))) 
        '((cjump 5 = 1 :_lablei1 :_lablei0) 
          :_lablei1 (eax <- (print 5)) (eax <- eax) (return) 
          :_lablei0 (eax <- (print test)) (eax <- eax) (return)))
  (test (compile-e '(let ((x 3))
                      (print x))) 'what))|#

;;compile L3
(define (compile-L3)
  (let ([L2-exp
         (append `(((call :_main_func)))
                 (for/list ([L3_func L3-exp]
                            [x (range (length L3-exp))])
                   (let ([L2_sub (compile-p L3_func)])
                     (if (equal? x 0) ;main function
                         (append `(:_main_func) L2_sub);(remove* `((return)) L2_sub)
                         L2_sub))))])
    (displayln L2-exp)))
(compile-L3)
