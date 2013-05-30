#lang plai
(require "L5-e.rkt")
(require "variable-util.rkt")
(require "temp-util.rkt")
(require data/queue)
;function to find free variables
(define func-que (make-queue))
(define env (list))

;;create new procedure
(define (new-procedure label x_lst var_lst exp)
  (if (> (length x_lst) 2)
      `(,label (vars-tup args-tup)
               ,(let ([new_exp (new-let x_lst exp 'args-tup)])
                  (new-let var_lst new_exp 'vars-tup)))
      `(,label ,(cons 'vars-tup x_lst)
               ,(new-let var_lst exp 'vars-tup))))

(define (new-let var_lst exp vars-tup)
  (let ([new_let exp])
    (for/fold ([new_let new_let]) 
      ([var (in-list var_lst)]
       [num (in-list (range (length var_lst)))])
      `(let ([,var (aref ,vars-tup ,num)]) ,new_let))))
;;create new lambda. e.g (f +) -> (f (lambda (x y) (+ x y))
(define (new-lambda e)
  (let ([prim_op (L5-compile e)]
        [x (fresh-var 'lam_x)]
        [y (fresh-var 'lam_y)]
        [z (fresh-var 'lam_z)])
    (cond [(or (biop? prim_op) 
               (equal? prim_op 'new-array)
               (equal? prim_op 'aref))
           (begin (back-one)
                  (L5-compile (L5_lambda `(,x ,y) (L5-parse `(,prim_op ,x ,y)))))]
          [(equal? prim_op 'aset)
           (L5-compile (L5_lambda `(,x ,y ,z) (L5-parse `(,prim_op ,x ,y ,z))))]
          [else
           (begin
             (back-one)
             (back-one)
             (L5-compile (L5_lambda `(,x) (L5-parse `(,prim_op ,x)))))])))

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
;(test (new-lambda (L5_prim 'print)) ""))
(define (L5-parse L5-e)
  (match L5-e
    [`(lambda ,args ,e)
     (L5_lambda args (L5-parse e))]
    [`(let ((,var ,e1)) ,e2)
     (let* ([level_var (level-var var)]
            [new_e2 (replace e2 var level_var)])
       (L5_let level_var (L5-parse e1) (L5-parse new_e2)))]
    [`(letrec ((,x ,e1)) ,e2)
     (let* ([level_x (level-var x)]
            [new_e1 (replace e1 x level_x)]
            [new_e2 (replace e2 x level_x)])
       (L5_letrec level_x (L5-parse new_e1) (L5-parse new_e2)))]
    ;(L5_let x (L5-parse `(new-tuple 0)) 
    ;        (L5_begin (L5-parse `(aset ,x 0 ,(replace e1 x `(aref ,x 0))))
    ;                  (L5-parse (replace e2 x `(aref ,x 0)))))] 
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
                                                      (L5_app (list (L5_prim 'aset) (L5_x 'x_1) (L5_num 0) (L5_num 5)))
                                                      (L5_app (list (L5_prim '+) 
                                                                    (L5_app (list (L5_prim 'aref) 
                                                                                  (L5_x 'x_1) (L5_num 0))) (L5_num 2))))))
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
               (let* ([new_lab (label-it (fresh-app))]
                      [e_compiled (L5-compile e)]
                      [free_var (find-free-var e x_lst)]
                      [clean_free_var (remove-duplicates (flatten free_var))])
                 (begin
                   (enqueue! func-que (new-procedure new_lab x_lst clean_free_var e_compiled)) 
                   `(make-closure ,new_lab ,(cons 'new-tuple clean_free_var)))))
    ;(set! env (remove-duplicates (append x_lst env))))))
    (L5_num (num)
            num)
    (L5_prim (prim)
             prim)
    (L5_x (x)
          x)
    (L5_let (x e1 e2)
            (if (L5_prim? e1)
                `(let ([,x ,(new-lambda e1)])
                   ,(L5-compile e2))
                #|(type-case L5-e e1
              (L5_lambda (x_lst e);if e1 is a lambda
                         (let ([new_lab (label-it x)]
                               [free_var (find-free-var x_lst e)])
                           `(let ([,x (make-closure ,new_lab ,(cons 'new-tuple free_var))])
                              ,(L5-compile e2)
                              ,(replace (L5-compile e2) x new_lab))))|#
                (begin ;(set! env (remove-duplicates (cons x env)))
                  `(let ([,x ,(L5-compile e1)])
                     ,(L5-compile e2)))))
    (L5_letrec (x e1 e2)
               ;(begin ;(set! env (remove-duplicates (cons x env)))
               `(let ((,x (new-tuple 0)))
                  (begin (aset ,x 0 ,(replace (L5-compile e1) x `(aref ,x 0)))
                         ,(replace (L5-compile e2) x `(aref ,x 0)))))
    ;(L5-compile `(L5_begin (aset ,x 0 ,(replace e1 x `(aref ,x 0)))
    ;(replace e2 x `(aref ,x 0))))))
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
            (let ([fun_e0 (L5-compile (first e_lst))])
              (if (prim? fun_e0);predefined function such as print, + etc, just left it as it is 
                  (map (λ (e)
                         (L5-compile e))
                       e_lst)
                  (let ([args (rest e_lst)])
                    (if (<= (length args) 2)
                        (let ([name (fresh-app)])
                          `(let ([,name ,(L5-compile (first e_lst))])
                             ,(append `((closure-proc ,name) (closure-vars ,name)) (map (λ (e)
                                                                                          (if (L5_prim? e)
                                                                                              (new-lambda e)
                                                                                              (L5-compile e)))
                                                                                        args))))
                        (let* ([app_name (fresh-app)]
                               [label (label-it app_name)])
                          `(let ([,app_name ,(L5-compile (first e_lst))])
                             ((closure-proc ,app_name) (closure-vars ,app_name)
                                                       ,(cons 'new-tuple (map (λ (ele)
                                                                                (if (L5_prim? ele)
                                                                                    (new-lambda ele)
                                                                                    (L5-compile ele)))
                                                                              args))))))))))
    ))
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
  (test (L5-compile (L5-parse `(let ((a 1)) a))) '(let ((a_1 1)) a_1))
  (test (L5-compile (L5-parse `(letrec ([x (+ x 1)]) (+ x 2)))) 
        '(let ((x_1 (new-tuple 0))) (begin (aset x_1 0 (+ (aref x_1 0) 1)) (+ (aref x_1 0) 2))))
  (test (L5-compile (L5-parse `(if (< x 1) (+ x 2) (+ x 3)))) '(if (< x 1) (+ x 2) (+ x 3)))
  (test (L5-compile (L5-parse `(let ([f (lambda (y) (+ x y))])
                                 (f 1))))  '(let ((f_1 (make-closure :new_app1 (new-tuple x))))
                                              (let ((new_app2 f_1)) 
                                                ((closure-proc new_app2) (closure-vars new_app2) 1)))))
(define (find-free-var exp arg_lst)
  (type-case L5-e exp
    (L5_lambda (x_lst e)
               (find-free-var e (flatten (cons x_lst arg_lst))))
    (L5_x (x)
          (if (not (member x arg_lst))
              x
              '()))
    (L5_let (x e1 e2)
            (cons (find-free-var e2 (flatten (cons arg_lst x)))
                  (find-free-var e1 arg_lst)))
    (L5_letrec (x e1 e2)
               (cons (find-free-var e2 (flatten (cons arg_lst x)))
                     (find-free-var e1 (flatten (cons arg_lst x)))))
    (L5_if (e1 e2 e3)
           (append (find-free-var e1 arg_lst)
                   (find-free-var e2 arg_lst)
                   (find-free-var e3 arg_lst)))
    (L5_new-tuple (e)
                  (for/list ([e_single e]) 
                    (find-free-var e_single arg_lst)))
    
    (L5_begin (e1 e2)
              (cons (find-free-var e1 arg_lst)
                    (find-free-var e2 arg_lst)))
    (L5_app (e_lst)
            (for/list ([e_single e_lst]) 
              (find-free-var e_single arg_lst)))
    (else '())))