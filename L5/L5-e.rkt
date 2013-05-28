#lang plai
(require "variable-util.rkt")

;;e for L5
#|(define (L5-e? exp)
  (match exp
    [(or `(lambda (,(? var?) ...) ,(? L5-e?))
        (? number?)
        (? var?)
        `(let ((,(? var?) ,(? L5-e?))) ,(? L5-e?))
        `(letrec ((,(? var?) ,(? L5-e?))) ,(? L5-e?))
        `(if ,(? L5-e?) ,(? L5-e?) ,(? L5-e?))
        `(new-tuple ,(? L5-e?) ...)
        `(begin ,(? L5-e?) ,(? L5-e?))
        `(,(? L5-e?) ...)
        (? prim?))
     #t]
    [_ #f]))
(module+ test
  (test (var? 'print) #f)
  (test (L5-e? 1) #t)
  (test (L5-e? `(+ 1 2)) #t)
  (test (L5-e? `(let ((1 2)) 2)) #f)
  (test (L5-e? `(lambda (x y z) x)) #t)
  (test (L5-e? `(lambda (1 print) 1)) #f)
  (test (L5-e? `(let let let)) #f))
|#        

(define-type L5-e
  (L5_lambda (x_lst (listof var?))
             (e L5-e?))
  (L5_num (num number?))
  (L5_x (x var?))
  (L5_let (x var?)
          (e1 L5-e?)
          (e2 L5-e?))
  (L5_letrec (x var?)
             (e1 L5-e?)
             (e2 L5-e?))
  (L5_if (e1 L5-e?)
         (e2 L5-e?)
         (e3 L5-e?))
  (L5_new-tuple (e (listof L5-e?)))
  (L5_begin (e1 L5-e?)
            (e2 L5-e?))
  (L5_app (e_lst (listof L5-e?)))
  ;biop pred
  (L5_prim (prim prim?)))

(module+ test
  (test (L5-e? (L5_new-tuple `(,(L5_num 4) ,(L5_x 'a) ,(L5_x 'd) ,(L5_x 'e)))) #t))