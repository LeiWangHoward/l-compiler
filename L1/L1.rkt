#lang plai
(require racket/string)
(require racket/file)
;read L1 code into the system
(define filename (command-line #:args (filename) filename))
(define L1-exp (call-with-input-file filename read))
;;define data type and operations
(define-type L1
  ;define basic data types, "a" means assemble-like language(L1)
  (reg_a (name symbol?))
  (lab_a (name symbol?))
  (imm_a (num number?));immediator, we use it to represent number
  ;define assign, which we call it move 
  (mov_a (lhs L1?)
         (rhs L1?))
  ;define memory operation: access and update
  (rmem_a (lhs L1?)
          (rhs L1?)
          (posi L1?))
  (wmem_a (lhs L1?)
          (rhs L1?)
          (posi L1?))
  ;define aop(arith operators)
  (add_a (lhs L1?)
         (rhs L1?))
  (sub_a (lhs L1?)
         (rhs L1?))
  (and_a (lhs L1?)
         (rhs L1?))
  (mul_a (lhs L1?)
         (rhs L1?))
  ;define sop(shift operators)
  (ls_a  (lhs L1?)
         (rhs L1?))
  (rs_a  (lhs L1?)
         (rhs L1?))
  ;define cmp(compare)
  (cmp_a (dest L1?)
         (cond1 L1?)
         (op symbol?)
         (cond2 L1?))
  ;define goto and cjump
  (goto_a  (label L1?))
  (cjmp_a  (cond1 L1?)
           (op symbol?)
           (cond2 L1?)
           (label1 L1?)
           (label2 L1?))
  ;define call and tail call
  (call_a  (u L1?))
  (tcall_a (u L1?))
  ;define return, print
  (return_a)
  (print_a (prt L1?))
  ;define allocate(init) spsce
  (alloc_a (x1 L1?)
           (x2 L1?))
  ;define array error
  (aerr_a  (x1 L1?)
           (x2 L1?)))


;; now we add actual "compile" functions 
(define (compile L1-func)
  (type-case L1 L1-func
    ;compile basic data type
    (reg_a (name) (string-append "%" (symbol->string name)))
    ;compile immediate data type(for constant value), add '$' to front   
    (imm_a (num)  (string-append "$" (number->string num)))
    (lab_a (name) (string-append "_" (substring (symbol->string name) 1)));(lab_a (name) (string-append "$" (substring (symbol->string name) 1)))
    ;compile assign
    (mov_a (lhs rhs)
            (let* ([new-l (compile rhs)]
                   [new-r (compile lhs)]);switch position
              (when (equal? #\_ (string-ref new-l 0))
              (set! new-l (string-append "$" new-l)));add back $, some label needs
             (string-append "movl " new-l ", " new-r)))
    ;compile memory access and update
    (rmem_a (lhs rhs posi); read memory
            (let* ([new-lhs (compile lhs)]
                   [new-rhs (compile rhs)]
                   [new-posi (number->string (imm_a-num posi))])
              (string-append "movl " new-posi "(" new-lhs ")" ", " new-rhs)))
    (wmem_a (lhs rhs posi); write memory
            (let* ([new-lhs (compile lhs)]
                   [new-rhs (compile rhs)]
                   [new-posi (number->string (imm_a-num posi))])
              (when (equal? #\_ (string-ref new-lhs 0))
                (set! new-lhs (string-append "$" new-lhs)))
              (string-append "movl " new-lhs ", " new-posi "(" new-rhs ")")))
    ;compile aop
    (add_a (lhs rhs)
            (let* ([l (compile rhs)]
                   [r (compile lhs)])
             (string-append "addl " l ", " r)))
    (sub_a (lhs rhs)
            (let* ([l (compile rhs)]
                   [r (compile lhs)])
             (string-append "subl " l ", " r)))
    (and_a (lhs rhs)
            (let* ([l (compile rhs)]
                   [r (compile lhs)])
             (string-append "andl " l ", " r)))
    (mul_a (lhs rhs)
            (let* ([l (compile rhs)]
                   [r (compile lhs)])
             (string-append "imull " l ", " r)))
    ;compile sop 
    (ls_a (lhs rhs)
           (let* ([l (compile rhs)]
                  [r (compile lhs)])
              (unless (eq? #\$ (string-ref l 0))
                 (set! l "%cl"));tricky part, use "small" register
              (string-append "sall " l ", " r)))
    (rs_a (lhs rhs)
           (let* ([l (compile rhs)]
                  [r (compile lhs)])
              (unless (eq? #\$ (string-ref l 0))
                 (set! l "%cl"))
              (string-append "sarl " l ", " r)))
    ;compile cmp
    (cmp_a (dest cond1 op cond2)
          (let* ([new-cond1 (compile cond2)]
                 [new-cond2 (compile cond1)]
                 [new-dest  (compile dest)]
                 [new-low-dest (lbits (compile dest))])
            (cond
              [(and (imm_a? cond1) (imm_a? cond2))
               (if (compare_a (imm_a-num cond1) op (imm_a-num cond2))
                   (string-append "mov $1, " new-dest);represent true
                   (string-append "mov $0, " new-dest))];represent false
              [(and (imm_a? cond1) (reg_a? cond2))
               (string-append "cmpl " new-cond2 ", " new-cond1 "\n"
                              (evaluate-negop op) new-low-dest "\n"
                              "movzbl " new-low-dest ", " new-dest)]
              [(or (and (reg_a? cond1) (imm_a? cond2))
                   (and (reg_a? cond1) (reg_a? cond2)))
               (string-append "cmpl " new-cond1 ", " new-cond2 "\n"
                              (evaluate-op op) new-low-dest "\n"
                              "movzbl " new-low-dest ", " new-dest)])))
    (goto_a (label)
           (let ([label1 (compile label)])
             (string-append "jmp " label1)));"jmp _" (substring label1 1))))
    (cjmp_a (cond1 op cond2 label1 label2)
           (cond
             [(and (imm_a? cond1) (imm_a? cond2));;situation 1, two numbers, jump
              (if (compare_a (imm_a-num cond1) op (imm_a-num cond2))
                  (string-append "jmp " (compile label1))
                  (string-append "jmp " (compile label2)))]
             [(and (imm_a? cond1) (reg_a? cond2))
              (let* ([new-l (compile cond1)]
                     [new-r (compile cond2)])
                (string-append "cmpl " new-l ", " new-r "\n"
                               (rev_jmp_a op (compile label1)
                                          (compile label2))))]
             [else
              (let* ([new-l (compile cond2)]
                     [new-r (compile cond1)])
                (string-append "cmpl " new-l ", " new-r "\n"
                               (normal_jmp_a op (compile label1)
                                             (compile label2))))]))
    ;compile call and tail-call
    ;reference lec3.txt
    (call_a (u) 
            (let ([label (symbol->string (gensym "_newlab"))])
                 (string-append "pushl $" label "\n"
                                "pushl %ebp\n"
                                "movl %esp, %ebp\n"
                                "jmp " (compile u) "\n";star??
                                label ":")))
    (tcall_a (u) (string-append "movl %ebp, %esp\n"
                                "jmp " (compile u)));star *??
    ;compile print
    (print_a (prt) (string-append "pushl " (compile prt) "\n"
                                  "call print\n"
                                  "addl $4, %esp"))
    ;compile alloc and array error
    (alloc_a (x1 x2) (string-append "pushl " (compile x2) "\n"
                                    "pushl " (compile x1) "\n"
                                    "call allocate\n"
                                    "addl $8, %esp"))
    (aerr_a (x1 x2) (string-append  "pushl " (compile x2) "\n"
                                    "pushl " (compile x1) "\n"
                                    "call print_error\n"
                                    "addl $8, %esp"))
    (return_a () "movl %ebp, %esp\npopl %ebp\nret")))

;;helper functions handle cmp and jmp
(define (compare_a x1 op x2)
  (case op
    [(<) (< x1 x2)]
    [(<=)(<= x1 x2)]
    [(=) (= x1 x2)]))

(define (normal_jmp_a op l1 l2)
  (case op
    [(<)  (string-append "jl " l1 "\njmp " l2)]
    [(<=) (string-append "jle " l1 "\njmp " l2)]
    [(=) (string-append "je " l1 "\njmp " l2)]))

(define (rev_jmp_a op l1 l2)
  (case op
    [(<) (string-append "jg " l1 "\njmp " l2)]
    [(<=) (string-append "jge " l1 "\njmp " l2)]
    [(=) (string-append "je " l1 "\njmp " l2)]))

(define (evaluate-op op)
  (case op
    [(<) (string-append "setl ")]
    [(<=) (string-append "setle ")]
    [(=) (string-append "sete ")]))

(define (evaluate-negop op)
  (case op
    [(<) (string-append "setg ")]
    [(<=) (string-append "setge ")]
    [(=) (string-append "sete ")]))

(define (lbits reg)
  (string-append "%" (substring reg 2 3) "l"))

;; Symbolic expressions(L1) to assemble language(x86) 
(define (parse sexp)
  (cond
    [(number? sexp) (imm_a sexp)]
    [(pair? sexp)
     (cond
       [(and (eq? 5 (length sexp))
             (or (symbol=? '< (fourth sexp))
                 (symbol=? '<= (fourth sexp))
                 (symbol=? '= (fourth sexp))))
        (cmp_a (parse (first sexp))
               (parse (third sexp))
               (fourth sexp)
               (parse (fifth sexp)))]
       [(eq? 3 (length sexp))
        ; when the line contains three elements
        (cond
          [(list? (first sexp));(mem x n4) <- s
           (wmem_a (parse (third sexp));from
                   (parse (second (first sexp)));to
                   (parse (third (first sexp))))]
          [(list? (third sexp))
           (case (first (third sexp))
             [(mem);s -> (mem x n4)
              (rmem_a (parse (second (third sexp)))
                      (parse (first sexp))
                      (parse (third (third sexp))))]
             [(print);x <- (print t)
              (print_a (parse (second (third sexp))))]
             [(allocate) ; x <- (allocate t t)
              (alloc_a (parse (second (third sexp)))
                       (parse (third (third sexp))))]
             [(array-error)
              (aerr_a (parse (second (third sexp)))
                      (parse (third (third sexp))))])]
          [else
           (case (second sexp)
             [(+=)
              (add_a (parse (first sexp))
                     (parse (third sexp)))]
             [(-=)
              (sub_a (parse (first sexp))
                     (parse (third sexp)))]
             [(&=)
              (and_a (parse (first sexp))
                     (parse (third sexp)))]
             [(*=)
              (mul_a (parse (first sexp))
                     (parse (third sexp)))]
             [(<-); simple move, e.g: edi <- eax
              (mov_a  (parse (first sexp))
                      (parse (third sexp)))]
             [(<<=)
              (ls_a  (parse (first sexp))
                     (parse (third sexp)))]
             [(>>=)
              (rs_a (parse (first sexp))
                    (parse (third sexp)))])])]
          ; now handle all the other situations     
    [(symbol=? 'cjump (first sexp))
     (cjmp_a (parse (second sexp))
             (third sexp)
             (parse (fourth sexp))
             (parse (fifth sexp))
             (parse (sixth sexp)))]
    [(symbol=? 'goto (first sexp))
     (goto_a (parse (second sexp)))]
    [(symbol=? 'call (first sexp))
     (call_a (parse (second sexp)))]
    [(symbol=? 'tail-call (first sexp))
     (tcall_a (parse (second sexp)))]
    [(symbol=? 'return (first sexp))
     (return_a)])]
    [else 
     (if (eq? #\: (string-ref (symbol->string sexp) 0))
         (lab_a sexp)
         (reg_a sexp))]))


(define header "\t.text\n\t.globl go\n\t.type  go, @function\ngo:\n")
(define footer "\t.size  go, .-go\n\t.section\t.note.GNU-stack,\"\",@progbits\n")

(define (main)
  (begin (when (file-exists? "prog.s")
           (delete-file "prog.s"))
         (with-output-to-file "prog.s" #:exists 'append
                             (lambda () (printf header)))
         (map (lambda (code_page) ;the whole page
                (begin 
                  (map (lambda (code_line);now map line by line
                         (let ([x86 (compile (parse code_line))])
                           (with-output-to-file "prog.s" #:exists 'append
                             (lambda () 
                               (begin 
                                 (when (eq? #\_ (string-ref x86 0))
                                   (set! x86 (string-append x86 ":")))
                                 (display (string-append x86 "\n")))))))
                  code_page)
                (when (eq? code_page (first L1-exp))
                           (with-output-to-file "prog.s" #:exists 'append
                             (lambda () (display "ret\n"))))))
              L1-exp)
         (with-output-to-file "prog.s" #:exists 'append
                             (lambda () (printf footer)))))

(main)
