#lang plai
(require racket/string)
(require "L1-type.rkt")
;read L1 code into the system
;(define filename "../322-interps/tests/40/1-test/tailcall.L1")
;(define filename "../322-interps/tests/robby/1-test/82.L1")
(define filename (command-line #:args (filename) filename))
(define L1_exp (call-with-input-file filename read))
;;define data type and operations

(define (add-prefix x)
  ;handle number first
  (if (number? x)
      (string-append "$" (number->string x))
      (let ([x_str (symbol->string x)])
        (if (L1_x? x);else it is a label
            (string-append "%" x_str)
            (string-append "$" x_str)))))

(define (jmp-label-name label)
  (string-append "_" (substring (symbol->string label) 1)))

;; define temp, label count and name
(define var_count -1)

(define (count-one)
  (begin
    (set! var_count (add1 var_count))
    var_count))

(define (aop-name op)
  (case op
    [(+=) "addl"]
    [(-=) "subl"]
    [(*=) "imull"]
    [(&=) "andl"]))

(define (sop-name op)
  (case op
    [(<<=) "sall"]
    [(>>=) "sarl"]))

(define (jmp-name op)
  (case op
    [(<)  "jl"]
    [(<=) "jle"]
    [(=) "je"]))

(define (rev-jmp-name op)
  (case op
    [(<)  "jg"]
    [(<=) "jge"]
    [(=) "je"]))
;evaluate number compariation in compiling time
(define (eval-compare x1 op x2)
  (case op
    [(<) (< x1 x2)]
    [(<=)(<= x1 x2)]
    [(=) (= x1 x2)]))
;define low bit name for register
(define (lbit-name reg)
  (case reg
    [(eax) "%al"]
    [(ecx) "%cl"]
    [(edx) "%dl"]
    [(ebx) "%bl"]))

(define (eval-op-name op)
  (case op
    [(<) "setl"]
    [(<=)"setle"]
    [(=) "sete"]))

(define (eval-negop-name op)
  (case op
    [(<) "setg"]
    [(<=) "setge"]
    [(=) "sete"]))

;; now we add actual "compile" functions 
(define (compile-i L1-inst)
  (type-case L1_i L1-inst
    (L1_assign (lhs rhs);switch position
               (let ([new_lhs (add-prefix rhs)]
                     [new_rhs (add-prefix lhs)])
                 (format "movl ~a, ~a" new_lhs new_rhs)))
    ;compile memory access and update
    (L1_rmem (lhs rhs); read memory
             (let ([new_lhs (add-prefix lhs)]
                   [new_x (add-prefix (second rhs))]
                   [new_n4 (number->string (third rhs))])
               (format "movl ~a(~a), ~a" new_n4 new_x new_lhs)))
    (L1_umem (lhs rhs); write memory
             (let ([new_rhs (add-prefix rhs)]
                   [new_x (add-prefix (second lhs))]
                   [new_n4 (number->string (third lhs))])
               (format "movl ~a, ~a(~a)" new_rhs new_n4 new_x)))
    ;compile aop
    (L1_aop (x op t)
            (let ([op_name (aop-name op)]
                  [new_l (add-prefix t)]
                  [new_r (add-prefix x)])
              (format "~a ~a, ~a" op_name new_l new_r)))
    ;compile sop 
    (L1_sop (x op sx)
            (let ([op_name (sop-name op)]
                  [new_l "%cl"];use small register
                  [new_r (add-prefix x)])
              (format "~a ~a, ~a" op_name new_l new_r)))
    (L1_sop2 (x op num)
             (let ([op_name (sop-name op)]
                   [new_l (add-prefix num)];use small register
                   [new_r (add-prefix x)])
               (format "~a ~a, ~a" op_name new_l new_r)))
    ;compile cmp
    (L1_cmp (cx t1 op t2)
            (let ([new-cx-l (lbit-name cx)]
                  [new-cx (add-prefix cx)]
                  [new-l (add-prefix t2)]
                  [new-r (add-prefix t1)])
              (cond
                [(and (number? t1) (number? t2))
                 (if (eval-compare t1 op t2)
                     (format "movl $1, ~a" (add-prefix cx))
                     (format "movl $0, ~a" (add-prefix cx)))]
                [(and (number? t1) (L1_x? t2))
                 (format "cmpl ~a, ~a\n~a ~a\nmovzbl ~a, ~a" new-r new-l 
                         (eval-negop-name op) new-cx-l new-cx-l new-cx)]
                [else
                 (format "cmpl ~a, ~a\n~a ~a\nmovzbl ~a, ~a" new-l new-r 
                         (eval-op-name op) new-cx-l new-cx-l new-cx)])))
    ;handle label goto, cjmp etc
    (L1_label (label) (string-append "_" (substring (symbol->string label) 1) ":"))
    (L1_goto (label)
             (let ([new_label (jmp-label-name label)])
               (format "jmp ~a" new_label)))
    (L1_cjmp (t1 op t2 label1 label2)
             (let ([new_tl (add-prefix t2)]
                   [new_tr (add-prefix t1)]
                   [new_label1 (jmp-label-name label1)]
                   [new_label2 (jmp-label-name label2)])
               (cond
                 [(and (number? t1) (number? t2));;situation 1, two numbers, jump
                  (if (eval-compare t1 op t2)
                      (format "jmp ~a" new_label1)
                      (format "jmp ~a" new_label2))]
                 [(and (number? t1) (L1_x? t2));;switch l and r
                  (format "cmpl ~a, ~a\n~a ~a\njmp ~a" new_tr new_tl (rev-jmp-name op) new_label1 new_label2)]
                 [else
                  (format "cmpl ~a, ~a\n~a ~a\njmp ~a" new_tl new_tr (jmp-name op) new_label1 new_label2)])))
    ;compile call and tail-call
    ;reference lec3.txt
    (L1_call (u) 
             (let ([label (string-append "_new_lab_" (number->string (count-one)))])
               (if (label? u)
                   (format "pushl $~a\npushl %ebp\nmovl %esp, %ebp\njmp ~a\n~a:" label (jmp-label-name u) label)
                   (format "pushl $~a\npushl %ebp\nmovl %esp, %ebp\njmp *~a\n~a:" label (add-prefix u) label))))
    
    (L1_tcall (u)
              (if (label? u)
                  (format "movl %ebp, %esp\njmp ~a" (jmp-label-name u))
                  (format "movl %ebp, %esp\njmp *~a" (add-prefix u))))
    
    ;compile print
    (L1_print (t) (format "pushl ~a\ncall print\naddl $4,%esp" (add-prefix t)))
    ;compile alloc and array error
    (L1_alloc (t1 t2) (format "pushl ~a\npushl ~a\ncall allocate\naddl $8,%esp" (add-prefix t2) (add-prefix t1)))
    (L1_aerr (t1 t2) (format "pushl ~a\npushl ~a\ncall print_error\naddl $8,%esp" (add-prefix t2) (add-prefix t1)))
    (L1_return () "movl %ebp, %esp\npopl %ebp\nret")))

;; Symbolic expressions(L1) to assemble language(x86) 
(define (L1-parse sexp)
  (if (label? sexp) 
      (L1_label sexp)
      (match sexp
        [`(,(? L1_x? x) <- ,(? L1_s? s)) (L1_assign x s)]
        [`(,(? L1_x? x1) <- ,(? mem_op? mem)) (L1_rmem x1 mem)]
        [`(,(? mem_op? mem) <- ,(? L1_s? s)) (L1_umem mem s)]
        [`(,x ,(? aop? op) ,t) (L1_aop x op t)]
        [`(,x ,(? sop? op) ecx) (L1_sop x op 'ecx)]
        [`(,x ,(? sop? op) ,(? number? num)) (L1_sop2 x op num)]
        [`(,(? L1_cx? cx) <- ,t1 ,(? cmp? op) ,t2) (L1_cmp cx t1 op t2)]
        [`(goto ,(? label? l)) (L1_goto l)]
        [`(cjump ,t1 ,cmp ,t2 ,l1 ,l2) (L1_cjmp t1 cmp t2 l1 l2)]
        [`(call ,u) (L1_call u)]
        [`(tail-call ,u) (L1_tcall u)]
        [`(return) (L1_return)]
        [`(eax <- (print ,t)) (L1_print t)]
        [`(eax <- (allocate ,t1 ,t2)) (L1_alloc t1 t2)]
        [`(eax <- (array-error ,t1 ,t2)) (L1_aerr t1 t2)])))

(define header "\t.text\n\t.globl go\n\t.type  go, @function\ngo:\n")
(define footer "\t.size  go, .-go\n\t.section\t.note.GNU-stack,\"\",@progbits\n")
(define main_prefix "pushl %ebp\nmovl %esp, %ebp\npushl %ebx\npushl %esi\npushl %edi\npushl %ebp\nmovl %esp, %ebp\n")
(define main_suffix "popl %ebp\npopl %edi\npopl %esi\npopl %ebx\nleave\nret\n")
(define (main)
  (begin
    (display header)
    (display main_prefix)
    (define check_main #t)
    (for ([L1_p L1_exp])
      (map (λ (L1_i);now map line by line
             (displayln (compile-i (L1-parse L1_i))))
           L1_p)
      (when (equal? check_main #t)
        (display main_suffix)
        (set! check_main #f)))
    (display footer)))
(main)