#lang plai
(require "L5-e.rkt")
(require "variable-util.rkt")
(require "temp-util.rkt")
(require data/queue)
(require racket/set)
;function to find free variables
(define func-que (make-queue))
;(define env (list))

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

;;parse L5-e
(define (L5-parse L5-e)
  (match L5-e
    [`(lambda ,args ,e)
     (let* ([new_args (map (λ (arg)
                             (level-var arg)) args)]
            [new_e (for/fold ([e e])
                     ([var (in-list new_args)]
                      [arg (in-list args)])
                     (replace e arg var))])
       (L5_lambda new_args (L5-parse new_e)))]
    [`(let ((,var ,e1)) ,e2)
     (let* ([level_var (level-var var)]
            [new_e2 (replace e2 var level_var)])
       (L5_let level_var (L5-parse e1) (L5-parse new_e2)))]
    [`(letrec ((,x ,e1)) ,e2)
     (let* ([level_x (level-var x)]
            [new_e1 (replace e1 x level_x)]
            [new_e2 (replace e2 x level_x)])
       (L5_letrec level_x (L5-parse new_e1) (L5-parse new_e2)))]
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

;;compile L5-expressions
(define (L5-compile L5-parsed)
  (type-case L5-e L5-parsed
    (L5_lambda (x_lst e)
               (let* ([new_lab (label-it (fresh-app))]
                      [e_compiled (L5-compile e)]
                      [free_var (find-free-var e (list->set x_lst))]
                      [free_var_lst (set->list free_var)])
                 (begin
                   (enqueue-front! func-que (new-procedure new_lab x_lst free_var_lst e_compiled))
                   `(make-closure ,new_lab ,(cons 'new-tuple free_var_lst)))))
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
    ;bug: double letrec
    (L5_letrec (x e1 e2)
               (let* ([pre_func_len (queue-length func-que)]  
                      [e1_compiled (L5-compile e1)]
                      [e2_compiled (L5-compile e2)]
                      [post_func_len (queue-length func-que)]
                      [refactor_func (- post_func_len pre_func_len)])
                 (begin
                   (for ([count (in-range refactor_func)])
                     (let* ([func (dequeue! func-que)]
                            [new_fun (replace-free func x)])
                       (enqueue! func-que new_fun)))
                   ;(begin ;(set! env (remove-duplicates (cons x env)))
                   `(let ((,x (new-tuple 0)))
                      (begin (aset ,x 0 ,(replace-free e1_compiled x))
                             ,(replace-free e2_compiled x))))))
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
            (let ([fun_e0 (first e_lst)])
              (if (L5_prim? fun_e0);predefined function such as print, + etc, just left it as it is 
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
                          `(let ([,app_name ,(L5-compile fun_e0)])
                             ((closure-proc ,app_name) (closure-vars ,app_name)
                                                       ,(cons 'new-tuple (map (λ (ele)
                                                                                (if (L5_prim? ele)
                                                                                    (new-lambda ele)
                                                                                    (L5-compile ele)))
                                                                              args))))))))))
    ))
;;find free variables
(define (find-free-var exp arg_set)
  (type-case L5-e exp
    (L5_lambda (x_lst e)
               (find-free-var e (set-union (list->set x_lst) arg_set)))
    (L5_x (x)
          (if (set-member? arg_set x)
              (set)
              (set x)))
    
    (L5_let (x e1 e2)
            (set-union (find-free-var e2 (set-add arg_set x))
                       (find-free-var e1 arg_set)))
    
    (L5_letrec (x e1 e2)
               (set-union (find-free-var e2 (set-add arg_set x))
                          (find-free-var e1 (set-add arg_set x))))
    (L5_if (e1 e2 e3)
           (set-union (find-free-var e1 arg_set)
                      (find-free-var e2 arg_set)
                      (find-free-var e3 arg_set)))
    (L5_new-tuple (e)
                  (for/fold ([full_set (set)])
                    ([e_single e])
                    (set-union full_set (find-free-var e_single arg_set))))
    
    (L5_begin (e1 e2)
              (set-union (find-free-var e1 arg_set)
                         (find-free-var e2 arg_set)))
    (L5_app (e_lst)
             (for/fold ([full_set (set)])
                    ([e_single e_lst])
                    (set-union full_set (find-free-var e_single arg_set))))
    (else (set))))