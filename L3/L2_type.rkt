#lang plai

;;define data type and operations
(define-type L2
  ;define assign, which we call it move 
  (l2_assign (lhs L2?)
             (rhs L2?))
  ;define memory operation: access and update
  (read_mem (lhs L2?)
            (rhs L2?)
            (off L2?))
  (write_mem (lhs L2?)
             (rhs L2?)
             (off L2?))
  ;define aop(arith operators)
  (l2_add (lhs L2?)
          (rhs L2?))
  (l2_sub (lhs L2?)
          (rhs L2?))
  (l2_and (lhs L2?)
          (rhs L2?))
  (l2_mul (lhs L2?)
          (rhs L2?))
  ;define sop(shift operators)
  (l2_ls  (lhs L2?)
          (rhs L2?))
  (l2_rs  (lhs L2?)
          (rhs L2?))
  ;define cmp(compare)
  (l2_cmp (dest L2?)
          (cond1 L2?)
          (op symbol?)
          (cond2 L2?))
  ;define goto and cjump
  (l2_goto  (label L2?))
  (l2_cjmp  (cond1 L2?)
            (op symbol?)
            (cond2 L2?)
            (label1 L2?)
            (label2 L2?))
  ;define call and tail call
  (l2_call  (u L2?))
  (l2_tcall (u L2?))
  ;define return, print
  (l2_return)
  (l2_print (prt L2?))
  ;define allocate(init) spsce
  (l2_alloc (x1 L2?)
            (x2 L2?))
  ;define array error
  (l2_aerr  (x1 L2?)
            (x2 L2?)))


;; now we add actual "compile" functions 
(define (compile L2-func)
  (type-case L2 L2-func
    ;compile basic data type
    (l2_reg (name) (name))))
    ;compile immediate data type(for constant value), add '$' to front   

;;helper functions handle cmp and jmp
(define (compare_a x1 op x2)
  (case op
    [(<) (< x1 x2)]
    [(<=)(<= x1 x2)]
    [(=) (= x1 x2)]))




