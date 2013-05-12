#lang plai
(require "L3-type.rkt")
(require "tools.rkt")
;(define (compile-e exp)


(define (compile-x-d x assign_exp)
  (type-case L3-d assign_exp
    (L3_biop (op v_1 v_2)
             `((,x <- ,v_1)
               (,x ,op ,v_2)
               (,x -= 1)))
    (else 'shit)))


(define (parse L3_func)
  (cond [(number? L3_func)
         (v_num L3_func)]
        [(label? L3_func)
         (v_label L3_func)]
        [(L3-var? L3_func)
         (v_var L3_func)]
        [(list? L3_func)
         (match L3_func
           ;; Handle L3 let  
           [`(let ((,x ,t)) ,e)
            (L3_let (parse x)
                    (parse t) (parse e))]
           ;; handle L3 biop
           [`(,biop ,v_1 ,v_2)
            (L3_d (L3_biop biop
                           (parse v_1)
                           (parse v_2)))])]
        [else #f]))

(define (compile L3_type)
  (type-case L3-e exp
    (L3_let (x d e) (compile-x-d x d))
    (L3_d (d) 'shit)
    (else 'shit))) 
#|  (cond [(number? func)
         (compile-v (v_num func))]
        [(label? func)
         (compile-v (v_label func))]
        [(list? func)
         (match func
           [`(let ((,x (,op ,y ,z))) ,ee)
            ;(L3_let `(,x ,x) `(,d (,L3_biop (,op ,y ,z))) `(,e ,ee))]
            `(,L3_let (,compile-v (,v_var ,x)) (,L3_biop (,op ,y ,z))
                      (,v_var ,ee))]
           [_ (compile-e func)])]
        [else func]))
|#
(module+ test
  (test (parse '(let ((a (+ b c))) (+ a 3))) 'hope))
;(test (compile (parse '(let ((a (+ b c))) (+ e 3)))) 'what))
