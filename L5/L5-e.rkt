#lang plai
(require "variable-util.rkt")     

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