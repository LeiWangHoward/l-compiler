#lang plai
(require "L5-e.rkt")
(require "variable-util.rkt")
(require "temp-util.rkt")
(require data/queue)
;function to find free variables
(define func-que (make-queue))
;;find free variable
(define (find-free-var x_lst e)
  (let* ([flat_e (flatten (L5-compile e))]
         [e_args (remove* x_lst flat_e)])
    (flatten (for/list
                 ([arg e_args])
               (if (var? arg)
                   arg
                   '())))))

;;create new procedure
(define (new-procedure label var_lst exp)
  `(,label ,(cons 'vars-tup var_lst)
           ,(let ([new_let exp])
             (for/fold ([new_let new_let]) 
               ([var (in-list var_lst)]
                [num (in-list (range (length var_lst)))])
               `(let ([,var (aref vars-tup ,num)]) ,new_let)))))

(module+ test
  (test (new-procedure ':f1 '(x y) '(- (+ x y) x)) '(:f1 (vars-tup x y) 
                                                         (let ((y (aref vars-tup 1)))
                                                           (let ((x (aref vars-tup 0))) 
                                                             (- (+ x y) x))))))
(define (L5-parse L5-e)
  (match L5-e
    [`(lambda ,args ,e)
     (L5_lambda args (L5-parse e))]
    [`(let ((,var ,e1)) ,e2)
     (L5_let var (L5-parse e1) (L5-parse e2))]
    [`(letrec ((,x ,e1)) ,e2)
     (L5_let x (L5-parse `(new-tuple 0)) 
             (L5_begin (L5-parse `(aset ,x 0 ,(replace e1 x `(aref ,x 0))))
                       (L5-parse (replace e2 x `(aref ,x 0)))))] 
    ;(L5_letrec var (L5-parse e1) (L5-parse e2))]
    [`(if ,cond ,then ,else)
     (L5_if (L5-parse cond)
            (L5-parse then)
            (L5-parse else))]
    [`(new-tuple ,args ...)
     (L5_new-tuple (map (λ (arg)
                          (L5-parse arg))
                        args))]
    [`(begin ,e1 ,e2)
     (L5_begin (L5-parse e1)
               (L5-parse e2))]
    [(? number? num)
     (L5_num num)]
    [(? prim? prim)
     (L5_prim prim)]
    [(? var? x)
     (L5_x x)]
    [`(,args ...)
     (L5_app (map (λ (arg)
                    (L5-parse arg))
                  args))]))

(module+ test 
  (test (L5-parse `(new-tuple 1 2 3 4)) (L5_new-tuple (list (L5_num 1) (L5_num 2) (L5_num 3) (L5_num 4))))
  (test (L5-parse `(me 1 2 3 4)) (L5_app (list (L5_x 'me) (L5_num 1) (L5_num 2) (L5_num 3) (L5_num 4))))
  (test (L5-parse `(lambda (x y z) (+ x y))) (L5_lambda '(x y z) (L5_app (list (L5_prim '+) (L5_x 'x) (L5_x 'y)))))
  (test (L5-parse `(letrec ((x 5)) (+ x 2))) (L5_let 'x (L5_new-tuple (list (L5_num 0)))
                                                     (L5_begin 
                                                      (L5_app (list (L5_prim 'aset) (L5_x 'x) (L5_num 0) (L5_num 5)))
                                                      (L5_app (list (L5_prim '+) (L5_app (list (L5_prim 'aref) (L5_x 'x) (L5_num 0))) (L5_num 2))))))
  (test (L5-parse `(if (< 1 2) (print 1) (print 2))) (L5_if (L5_app (list (L5_prim '<) (L5_num 1) (L5_num 2))) 
                                                            (L5_app (list (L5_prim 'print) (L5_num 1)))
                                                            (L5_app (list (L5_prim 'print) (L5_num 2)))))
  (test (L5-parse `(begin (+ x 1) (+ x 3))) (L5_begin (L5_app (list (L5_prim '+) (L5_x 'x) (L5_num 1))) 
                                                      (L5_app (list (L5_prim '+) (L5_x 'x) (L5_num 3)))))
  (test (L5-parse `(let ((a 1)) a)) (L5_let 'a (L5_num 1) (L5_x 'a))))

;;compile L5-expressions
(define (L5-compile L5-parsed)
  (type-case L5-e L5-parsed
    (L5_lambda (x_lst e)
               (let ([new_lab (label-it (fresh-app))]
                     [free_var (find-free-var x_lst e)])
                 (begin
                   (enqueue! func-que (new-procedure new_lab free_var)) 
                   `(make-closure ,new_lab ,(cons 'new-tuple free_var)))))
    (L5_num (num)
            num)
    (L5_prim (prim)
             prim)
    (L5_x (x)
          x)
    (L5_let (x e1 e2)
            (type-case L5-e e1
              (L5_lambda (x_lst e);if e1 is a lambda
                         (let ([new_lab (label-it x)]
                               [free_var (find-free-var x_lst e)])
                           `(let ([,x (make-closure ,new_lab ,(cons 'new-tuple free_var))])
                              ,(L5-compile e2)
                              ,(replace (L5-compile e2) x new_lab))))
              (else
               `(let ([,x ,(L5-compile e1)])
                  ,(L5-compile e2)))))
    ;(L5_letrec (x e1 e2) ;eliminated in parse step
    (L5_if (e1 e2 e3)
           `(if ,(L5-compile e1)
                ,(L5-compile e2)
                ,(L5-compile e3))) 
    (L5_new-tuple (e)
                  (cons 'new-tuple (map (λ (e_single)
                                          (L5-compile e_single))
                                        e)))
    (L5_begin (e1 e2)
              `(begin ,(L5-compile e1)
                      ,(L5-compile e2)))
    (L5_app (e_lst)
            (let ([fun_name (first e_lst)]) 
              (if (L5_prim? fun_name);predefined function such as print, + etc, just left it as it is 
                  (map (λ (e)
                         (L5-compile e))
                       e_lst) 
                  ;(label-it (L5-compile fun_name))
                  (let ([args (rest e_lst)])
                    (if (<= (length args) 2)
                        (let ([name (fresh-app)])
                          `(let ([,name ,(L5-compile (first e_lst))])
                             ,(append `((closure-proc ,name) (closure-vars ,name)) (map (λ (e)
                                                                                        (L5-compile e))
                                                                                      args))))
                        (let* ([app_name (fresh-app)]
                               [label (label-it app_name)])
                          `(let ([,app_name ,(L5-compile (first e_lst))])
                             ((closure-proc ,app_name) (closure-vars ,app_name)
                                                       (new-tuple ,(map (λ (ele)
                                                                          ele)
                                                                        args))))))))))
    (else L5-parsed)))
;(else L5-parsed)))
(module+ test 
  (test (L5-compile (L5-parse 'f)) 'f)
  (test (L5-parse 'f) (L5_x 'f))
  (test (L5-parse `(let ([f (lambda (y) (+ x y))])
                     (f 1))) 
        (L5_let 'f (L5_lambda '(y) 
                              (L5_app (list (L5_prim '+) (L5_x 'x) (L5_x 'y))))
                (L5_app (list (L5_x 'f) (L5_num 1)))))
  (test (L5-compile (L5-parse `(print 1))) '(print 1))
  (test (L5-compile (L5-parse `(f 1 2))) '(let ((new_app0 f)) ((closure-proc new_app0) (closure-vars new_app0) 1 2)))
  (test (L5-compile (L5-parse `(let ((a 1)) a))) '(let ((a 1)) a))
  (test (L5-compile (L5-parse `(letrec ([x (+ x 1)]) (+ x 2)))) 
        '(let ((x (new-tuple 0))) (begin (aset x 0 (+ (aref x 0) 1)) (+ (aref x 0) 2))))
  (test (L5-compile (L5-parse `(if (< x 1) (+ x 2) (+ x 3)))) '(if (< x 1) (+ x 2) (+ x 3)))
  (test (L5-compile (L5-parse `(let ([f (lambda (y) (+ x y))])
                                 (f 1)))) "")) 