#lang plai
;(require "L3-type.rkt")
(require "tools.rkt")

;; compile L3-d and several d functions
; simple biop compile
(define (compile-biop op v1 v2 x)
  (case op
    [(+)
     `((,x <- ,v1)
       (,x += ,v2)
       (,x -= 1))]
    [(-)
     `((,x <- ,v1)
       (,x -= ,v2)
       (,x += 1))]
    [(*)
     `((,x <- ,v1)
       (,x *= ,v2)
       (,x -= ,v1)
       (,x -= ,v2)
       (,x += 3)
       (,x >>= 1))]
    [(< <= =)
     `((,x <- ,v1 ,op ,v2)
       (,x += ,x)
       (,x += 1))]))
; compile-biop, this version controls the information of encode and decode
#|(define (compile-biop op v1 v2 x)
  (let* ([v1 (encode-const v1)]
         [v2 (encode-const v2)]
         [tmp2 (new-temp)])
    (add-encode x);;we know x will hold encoded result
    (case op
      [(+)
       (append
        (encode-if-var v1)
        `((,x <- ,v1))
        (encode-if-var v2)
        `((,x += ,v2))
        `((,x -= 1)))]
      ;`((eax <- ,x)))]
      [(-)
       (append
        (encode-if-var v1)
        `((,x <- ,v1))
        (encode-if-var v2)
        `((,x -= ,v2))
        `((,x += 1)))]
      ;`((eax <- ,x)))]
      [(*)
       (append
        (encode-if-var v1)
        `((,tmp2 <- ,v1))
        `((,tmp2 >>= 1))
        (encode-if-var v2)
        `((,x <- ,v2))
        `((,x >>= 1))
        `((,x *= ,tmp2))
        `((,x *= 2))
        `((,x += 1)))]
      ;`((eax <- ,x)))]
      ; < <= =
      [(< <= =)
       (append
        (encode-if-var v1)
        (encode-if-var v2)
        `((,x <- ,v1 ,op ,v2))
        `((,x <<= 1))
        `((,x += 1)))])))|#
;pred
(define (compile-pred pred v x)
  (let ([v (encode-const v)])
    (case pred
      [(a?)
       (append
        `((,x <- ,v))
        `((,x &= 1))
        `((,x *= -2))
        `((,x += 3)))]
      [(number?)
       (append
        `((,x <- ,v))
        `((,x &= 1))
        `((,x <<= 1))
        `((,x += 1)))])))
;alen
(define (compile-alen v x)
  (append `((,x <- (mem ,v))) ;; v can't be a constant here or 
          ;; else the L3 program doesn't work anyways.
          `((,x <<= 1))
          `((,x += 1))))

(module+ test 
  (test (compile-alen 'me 'x) '((x <- (mem me)) (x <<= 1) (x += 1))))
;aset
(define (compile-aset v1 v2 v3 x)
  (let ([v2 (encode-const v2)]
        [v3 (encode-const v3)]
        [tmp (new-temp)]
        [bounds-fail-label (new-fail)]
        [bounds-pass-label (new-pass)]
        [bounds-pass-label-2 (new-pass)])
    (append
     `((,x <- ,v2))
     `((,x >>= 1))
     `((,tmp <- (mem ,v1 0)))
     `((cjump ,x < ,tmp  ,bounds-pass-label ,bounds-fail-label))
     `(,bounds-pass-label)
     ;check if the reference is > 0
     `((cjump 0 <= ,v2 ,bounds-pass-label-2 ,bounds-fail-label))
     `(,bounds-fail-label)
     `((eax <- (array-error ,v1 ,v2)))
     `(,bounds-pass-label-2)
     `((,x *= 4))
     `((,x += ,v1))
     `(((mem ,x 4) <- ,v3))
     `((,x <- 1)))))

;aref
(define (compile-aref v1 v2 x)
  (let ([v2 (encode-const v2)]
        [tmp (new-temp)]
        [bounds-fail-label (new-fail)]
        [bounds-pass-label (new-pass)]
        [bounds-pass-label-2 (new-pass)])
    `((,x <- ,v2)
      (,x >>= 1)
      (,tmp <- (mem ,v1 0))
      (cjump ,x < ,tmp  ,bounds-pass-label ,bounds-fail-label)
      ,bounds-pass-label
      ;check if the reference is > 0
      (cjump 0 <= ,v2 ,bounds-pass-label-2 ,bounds-fail-label)
      ,bounds-fail-label
      (eax <- (array-error ,v1 ,v2))
      ,bounds-pass-label-2
      (,x *= 4)
      (,x += ,v1)
      (,x <- (mem ,x 4)))))
;(module+ test (compile-aref 5 7 'x))

;;compile d
(define (compile-d L2_d var_x check_tail)
  (match L2_d
    [`(,(? biop? op) ,v1 ,v2)
     (add-return-call (compile-biop op v1 v2 var_x) check_tail)]
    [`(,(? pred? a) ,v)
     (add-return-call (compile-pred a v var_x) check_tail)]
    [`(new-array ,v1 ,v2)
     (add-return-call
      `((eax <- (allocate ,(encode-const v1) ,(encode-const v2)))
        (,var_x <- eax)) check_tail)]
    [(list 'new-tuple args ...)
     (add-return-call
      (append `((eax <- (allocate ,(encode-const (length args)) 3)))
              `(,var_x <- eax)
              (make-mem-assigns var_x args)) check_tail)]
    [(list 'aref v1 v2)
     (add-return-call
      (compile-aref v1 v2 var_x) check_tail)]
    [(list 'aset v1 v2 v3)
     (add-return-call
      (compile-aset v1 v2 v3 var_x) check_tail)]
    [(list 'alen v)
     (add-return-call
      `((,var_x <- (mem ,v 0))
        (,var_x <<= 1)
        (,var_x += 1)) check_tail)]
    [(list 'print v)
     ;(if (number? v)
     (add-return-call
      `((eax <- (print ,(encode-const v)))
        (,var_x <- eax)) check_tail)]
    ;  `((eax <- (print ,(encode-const v)))
    ;    (,var_x <- eax)) check_tail)]
    ;    (let ([tmp (new-temp)])
    ;      (add-return-call
    ;       `((,tmp <- ,v)
    ;         (,tmp <<= 1)
    ;         (,tmp += 1)
    ;         (eax <- (print ,tmp))
    ;        (,var_x <- eax)) check_tail)))]
    [(list 'make-closure args ...);;same as new-turple
     (add-return-call
      (append `((eax <- (allocate ,(encode-const (length args)) 3))
                (,var_x <- eax))
              (make-mem-assigns var_x args)) check_tail)]
    [(list 'closure-proc v)
     (add-return-call
      (compile-aref v 0 var_x) check_tail)]
    [(list 'closure-vars v)
     (add-return-call 
      (compile-aref v 1 var_x) check_tail)]
    [(list (? label? f) args ...)
     (if (equal? check_tail #t)
         (append (assign-regs args);assign-regs also check if the register already has encoded value
                 `((tail-call ,f)))
         (append (assign-regs args)
                 `((call ,f))
                 `((,var_x <- eax))))]
    ;;constant?
    [_;(list (? symbol? L2_d))
     (add-return-call;;no need to encode here
      `((eax <- ,L2_d)) check_tail)]))
(module+ test
  (test (compile-d '(+ 2 'a) 't #t) '((t <- 5) (t += 'a) (t -= 1) (eax <- t) (return)))
  (test (compile-d '(:f var1 var2) 'var #t) '((ecx <- var1) (edx <- var2) (tail-call :f)))
  (test (compile-d '(:f var1 var2 var3) 'var #f) '((ecx <- var1) (edx <- var2) (eax <- var3) (call :f) (var <- eax)))
  ;(test (append `((symbol <- v1)) `((sd <- 5))) '((symbol <- v1) (sd <- 5))) 
  (test (compile-biop '+ 'a 3 '_lei) '((_lei <- a) (_lei += 7) (_lei -= 1) (eax <- _lei)))
  (test (compile-biop '< 'a 'b '_lei) '((_lei <- a < b) (_lei <<= 1) (_lei += 1)))
  (test (encode-const 5) 11)) 

;; compile L3-e
(define (compile-e L3_e)
  (match L3_e
    [`(let ((,x ,L3_d)) ,(? list? L3_e_2))
     (compile-let x L3_d L3_e_2)]
    [`(if ,v ,e1 ,e2)
     (compile-if v e1 e2)]
    [_ 
     (compile-d L3_e 'eax #t)]));'eax #t)]))
; compile let in L3-e
(define (compile-let var L3_d L3_e)
  (cond [(number? L3_d)
         (append `((,var <- ,(encode-const L3_d))) (compile-e L3_e))]
        [(symbol? L3_d)
         (append `();`((,L3_d *= 2) (,L3_d += 1))
                 `((,var <- ,L3_d)) (compile-e L3_e))]
        [else (append (compile-d L3_d var #f) (compile-e L3_e))]))
;compile if in L3-e
(define (compile-if v e1 e2)
  (let ([then (new-label)]
        [else (new-label)])
    ;[v (encode-const v)])
    (append
     `((cjump ,(encode-const v) = 1 ,else ,then))
     `(,else)
     (compile-e e2)
     `(,then)
     (compile-e e1))))

(module+ test 
  (test (compile-if 3 '(print a) '(print b)) 
        '((cjump 3 = 1 :_lablei4 :_lablei3) 
          :_lablei4 (eax <- (print b)) (eax <- eax) (return) 
          :_lablei3 (eax <- (print a)) (eax <- eax) (return)))
  (test (compile-let 'x 3 '(print 3)) '((x <- 7) (eax <- (print 7)) (eax <- eax))))
#|
(define (parse exp)
  (match exp
    [(? number?) (v_num exp)]
    [(? biop?) (v_biop exp)]
    [(? label?) (v_label exp)]
    [(? L3-var?) (v_var exp)]
    [`{let ((,x ,t)) ,e}
     (L3_let (parse x)
             (parse t) 
             (parse e))]
    [`{,(? biop? op) ,a ,b} (L3_biop (parse op)
                                     (parse a) 
                                     (parse b))]))

(define (compile L3_type)
  (type-case L3-d L3_type
    ;(L3_let (x d e) (compile-x-d x d))
    (L3_biop (op v_1 v_2)
             (list 'x '<- (compile-v v_1)))
               ;(x ,op (,compile-v ,v_2))
               ;(x -= 1)))
    ;(L3_d (d) (compile-x-d 'a d))
    (else 'waht)))|# 
#|  (cond [(number? func)
         (compile-v (v_num func))]
        [(label? func)
         (compile-v (v_label func))]
        [(list? func)
         (match func
           [`(let ((,x (,op ,y ,z))) ,ee)
            ;(L3_let `(,x ,x) `(,d (,L3_biop (,op ,y ,z))) `(,e ,ee))]
            `(,L3_let (,compile-v (,v_var ,x)) (,L3_biop (,op ,y ,z))
                      (,v_var ,ee))]
           [_ (compile-e func)])]
        [else func]))
|#