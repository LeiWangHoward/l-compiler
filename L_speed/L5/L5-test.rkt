#lang plai

;;;L5-e
(require "L5-e.rkt")
(module+ test
  (test (L5-e? (L5_new-tuple `(,(L5_num 4) ,(L5_x 'a) ,(L5_x 'd) ,(L5_x 'e)))) #t))

;;;temp and var util
(require "temp-util.rkt")
(require "variable-util.rkt")
;;for temp
(module+ test
  (test (fresh-var 'x) 'var_x0)
  (test (label-it 'a) ':a)
  (test (level-var 'me) 'me_1)
  (test (fresh-app) 'new_app1)
  (test (line-filter 'n-of-p) 'n_of_p))
;;for determine var 
(module+ test
  (test (var? 'aa) #t)
  (test (var? 'sdsd?) #f)
  (test (var? '+) #f)
  (test (var? 'print) #f)
  (test (prim? '+) #t)
  (test (prim? 'print) #t)
  (test (prim? 'number?) #t))

;;test replace "free" x with (aref x 0) in letrec
(module+ test 
  (test (replace-free '(+ a b) 'a) '(+ (aref a 0) b))
  (test (replace-free '(make-closure :f (new-tuple a b c)) 'b) 
        '(make-closure :f (new-tuple a b c)))
  ;;x -> s0
  (test (replace '(+ 1 (+ 2 x)) 'x '(aref x 0)) '(+ 1 (+ 2 (aref x 0)))))

;;test new-procedure and its helper function
(require "L5-parse-compile.rkt")
(module+ test
  (test (new-procedure ':f1 '(x) '(y) '(- (+ x y) x)) '(:f1 (vars-tup x) 
                                                            (let ((y (aref vars-tup 0))) 
                                                              (- (+ x y) x))))
  (test (new-let '(y) '(- (+ x y) x) 'vars-tup)  '(let ((y (aref vars-tup 0))) 
                                                    (- (+ x y) x)))
  (test (new-procedure ':f1 '(x y z) '(p) '(- (+ (- (+ x y) x) z) p)) '(:f1 (vars-tup args-tup) 
                                                                            (let ((p (aref vars-tup 0))) 
                                                                              (let ((z (aref args-tup 2))) 
                                                                                (let ((y (aref args-tup 1))) 
                                                                                  (let ((x (aref args-tup 0))) 
                                                                                    (- (+ (- (+ x y) x) z) p))))))))

;;parse L5-e
(module+ test 
  (test (L5-parse `(new-tuple 1 2 3 4)) (L5_new-tuple (list (L5_num 1) (L5_num 2) (L5_num 3) (L5_num 4))))
  (test (L5-parse `(me 1 2 3 4)) (L5_app (list (L5_x 'me) (L5_num 1) (L5_num 2) (L5_num 3) (L5_num 4))))
  (test (L5-parse `(lambda (x y z) (+ x y))) (L5_lambda '(x_1 y_1 z_1) (L5_app (list (L5_prim '+) (L5_x 'x_1) (L5_x 'y_1)))))
  (test (L5-parse `(letrec ((x 5)) (+ x 2))) (L5_letrec 'x_1 (L5_num 5) (L5_app (list (L5_prim '+) (L5_x 'x_1) (L5_num 2)))))
  (test (L5-parse `(if (< 1 2) (print 1) (print 2))) (L5_if (L5_app (list (L5_prim '<) (L5_num 1) (L5_num 2))) 
                                                            (L5_app (list (L5_prim 'print) (L5_num 1)))
                                                            (L5_app (list (L5_prim 'print) (L5_num 2)))))
  (test (L5-parse `(begin (+ x 1) (+ x 3))) (L5_begin (L5_app (list (L5_prim '+) (L5_x 'x) (L5_num 1))) 
                                                      (L5_app (list (L5_prim '+) (L5_x 'x) (L5_num 3)))))
  (test (L5-parse `(let ((a 1)) a)) (L5_let 'a_1 (L5_num 1) (L5_x 'a_1)))
  
  (test (L5-parse 'f) (L5_x 'f))
  (test (L5-parse `(let ([f (lambda (y) (+ x y))])
                     (f 1))) 
        (L5_let 'f_1 (L5_lambda '(y_1) 
                                (L5_app (list (L5_prim '+) (L5_x 'x) (L5_x 'y_1))))
                (L5_app (list (L5_x 'f_1) (L5_num 1))))))

;;compile L5
(module+ test 
  ;;we reset the counter first
  (test (begin (reset-count) var_count) -1)
  (test (L5-compile (L5-parse 'f)) 'f)
  (test (L5-compile (L5-parse `(print 1))) '(print 1))
  (test (L5-compile (L5-parse `(f 1 2))) '(let ((new_app0 f)) ((closure-proc new_app0) (closure-vars new_app0) 1 2)))
  (test (L5-compile (L5-parse `(let ((a 1)) a))) '(let ((a_1 1)) a_1))
  (test (L5-compile (L5-parse `(letrec ([x (+ x 1)]) (+ x 2)))) 
        '(let ((x_1 (new-tuple 0))) (begin (aset x_1 0 (+ (aref x_1 0) 1)) (+ (aref x_1 0) 2))))
  (test (L5-compile (L5-parse `(if (< x 1) (+ x 2) (+ x 3)))) '(if (< x 1) (+ x 2) (+ x 3)))
  (test (L5-compile (L5-parse `(let ([f (lambda (y) (+ x y))])
                                 (f 1))))  '(let ((f_1 (make-closure :new_app1 (new-tuple x))))
                                              (let ((new_app2 f_1)) 
                                                ((closure-proc new_app2) (closure-vars new_app2) 1)))))

;;test find-free-var
(module+ test
  (test (find-free-var (L5-parse '(lambda (x y z) (- x q))) (set 'x 'y 'z)) (set 'q))
  (test (find-free-var (L5-parse '(lambda (n)
                                    (if (< n 2)
                                        1
                                        (+ (fib (- n 1))
                                           (fib (- n 2)))))) (set 'n)) (set 'fib)))

;;test optimization
(require "L5.rkt")
(module+ test
  (test (L5-compile (L5-parse '(letrec ([fib (lambda (n)
                                               (if (< n 2)
                                                   1
                                                   (+ (fib (- n 1))
                                                      (fib (- n 2)))))])
                                 (fib 30)))) '(:fib_1 30)))