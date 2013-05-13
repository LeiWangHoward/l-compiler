#lang plai
(require "tools.rkt")
;;v for L3
(define-type L3-v
  (v_var (var L3-var?))
  (v_label (label label?))
  (v_num (num number?)))

(define (compile-v v) 
  (type-case L3-v v 
    ;compile basic data type
    (v_var (var) var)
    (v_num (num) num)
    (v_label (label) label)))

;;biop and a? for L3
(define-type L3-op
  (v_biop (biop biop?)))
;;arglist for L3
(define-type L3-arglist
  (var_one (var_1 L3-var?))
  (var_two (var_1 L3-var?)
           (var_2 L3-var?))
  (var_three (var_1 L3-var?)
             (var_2 L3-var?)
             (var_3 L3-var?)))
;;p(program) for L3
(define-type L3-program
  (L3_e (exp L3-e?))
  (L3_func (label label?)
           (L3_arglist L3-arglist?)
           (L3_e L3_e?)))

;;d for L3
(define-type L3-d
  (L3_biop (biop v_biop?) 
           (v_1 L3-v?)
           (v_2 L3-v?))
  (L3_pred (pred symbol?)
           (v L3-v?))
  ;; fn call with 1 arg
  (L3_fn_1 (f L3-v?)
           (v L3-v?))     
  ;; fn call with 2 args
  (L3_fn_2 (f L3-v?)
           (v_1 L3-v?)
           (v_2 L3-v?)) 
  ;; fn call with 3 arg
  (L3_new-array (v_1 L3-v?)
                (v_2 L3-v?))
  (new-tuple (v_lst list?))
  (L3_aref (v_1 L3-v?)
           (v_2 L3-v?))
  (L3_aset (v_1 L3-v?)
           (v_2 L3-v?)
           (v_3 L3-v?))
  (L3_alen (v L3-v?))
  (L3_print (v L3-v?))
  (L3_make-closure (label v_label?)
                   (v L3-v?))
  (L3_closure-proc (v L3-v?))
  (L3_closure-vars (v L3-v?))
  (L3_v (v L3-v?)))

;;e for L3, combined with d
(define-type L3-e
  (L3_let ;(x_d L3-x-d-assign?)
   (x v_var?)
   (d L3-d?)
   (e L3-e?))
  (L3_if (v L3-v?)
         (e_1 L3_e?)
         (e_2 L3_e?))
  (L3_d (d L3-d?)))


;(define (compile func)
;  (type-case L3-arglist func
;    (var_one (var_1) (L3-v var_1))
;    (else "bad")))


;; now we add actual "compile" functions 
;(define (compile L2-func)
;  (type-case L2 L2-func
;compile basic data type
;    (l2_reg (name) (name))))
;compile immediate data type(for constant value), add '$' to front   

