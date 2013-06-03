#lang plai
(define (label? l)
  (if (symbol? l)
      (equal? (string-ref (symbol->string l) 0) #\:)
      #f))

(define (L1_x? x)
  (if (member x '(eax ebx ecx edx esi edi ebp esp))
      #t
      #f))

(define (L1_t? t)
  (or (L1_x? t) (number? t)))

(define (L1_u? u)
  (or (L1_x? u) (label? u)))

(define (L1_s? var)
  (or (symbol? var) (number? var))) 

(define (L1_sx? x)
  (equal? x 'ecx))

(define (L1_cx? x)
  (if (member x '(eax ebx ecx edx))
      #t
      #f))

(define (mem_op? r)
  (match r
    [`(mem ,(? L1_x?) ,(? number?)) #t]
    [_ #f]))

(define (aop? op)
  (if (member op (list '+= '-= '*= '&=))
      #t
      #f))

(define (sop? op)
  (if (member op (list '<<= '>>=))
      #t
      #f))

(define (cmp? op)
  (if (member op (list '<= '= '<))
      #t
      #f))

(define-type L1_i
  ;define assign, which we call it move 
  (L1_assign (lhs L1_x?)
             (rhs L1_s?))
  ;define memory operation: access and update
  (L1_rmem (lhs L1_x?)
           (rhs mem_op?))
  (L1_umem (lhs mem_op?)
           (rhs L1_s?))
  ;define aop(arith operators)
  (L1_aop (x L1_x?)
          (op aop?)
          (t L1_t?))
  ;define sop(shift operators)
  (L1_sop  (x L1_x?)
           (op sop?)
           (sx L1_sx?))
  (L1_sop2 (x L1_x?)
           (op sop?)
           (num number?))
  ;define cmp(compare)
  (L1_cmp (cx L1_cx?)
          (t1 L1_t?)
          (op cmp?)
          (t2 L1_t?))
  ;define goto and cjump
  (L1_label (label label?))
  (L1_goto  (label label?))
  (L1_cjmp  (t1 L1_t?)
            (op cmp?)
            (t2 L1_t?)
            (label1 label?)
            (label2 label?))
  ;define call and tail call
  (L1_call  (u L1_u?))
  (L1_tcall (u L1_u?))
  ;(return)
  (L1_return)
  ;; calls into runtime system
  ; one to print a value
  ; (returns the encoded version of 0, which gets stored in eax)
  (L1_print (t L1_t?))
  ;one to allocate & initialize some space
  ;(returns the location where the allocated memory begins)
  (L1_alloc (t1 L1_t?)
            (t2 L1_t?))
  ;one to signal a runtime error on array dereference and 
  ;terminate the program (doesn't actually return)
  (L1_aerr  (t1 L1_t?)
            (t2 L1_t?)))