#lang plai
;(require "L3-type.rkt")
(require "tools.rkt")

;; compile L3-d and several d functions
; simple biop compile
(define (compile-biop op v1 v2 x)
  (let ([v1 (encode-const v1)]
        [v2 (encode-const v2)])
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
         (,x <<= 1)
         (,x += 1))])))
;pred
(define (compile-pred pred v x)
  (let ([v (encode-const v)])
    (case pred
      [(a?)
       `((,x <- ,v)
         (,x &= 1)
         (,x *= -2)
         (,x += 3))]
      [(number?)
       `((,x <- ,v)
         (,x &= 1)
         (,x <<= 1)
         (,x += 1))])))
;alen
(define (compile-alen v x)
  `((,x <- (mem ,v)) ;; v can't be a constant here
    (,x <<= 1)
    (,x += 1)))
;test
(module+ test 
  (test (compile-alen 'me 'x) '((x <- (mem me)) (x <<= 1) (x += 1))))
;aset
(define (compile-aset v1 v2 v3 x)
  (let ([v2 (encode-const v2)]
        [v3 (encode-const v3)]
        [tmp (new-temp)]
        [tmp_2 (new-temp)]
        [bounds-fail-label (new-fail)]
        [bounds-pass-label (new-pass)]
        [bounds-pass-label-2 (new-pass)])
    `((,tmp_2 <- ,v2)
      ;(,x >>= 1)
      (,tmp <- (mem ,v1 0))
      (,tmp <<= 1)
      (,tmp += 1)
      (cjump ,tmp_2 < ,tmp  ,bounds-pass-label ,bounds-fail-label)
      ,bounds-pass-label
      ;check if the reference is > 0
      (cjump 0 <= ,tmp_2 ,bounds-pass-label-2 ,bounds-fail-label)
      ,bounds-fail-label
      (eax <- (array-error ,v1 ,v2))
      ,bounds-pass-label-2
      (,tmp_2 >>= 1)
      (,tmp_2 *= 4)
      (,tmp_2 += ,v1)
      ((mem ,tmp_2 4) <- ,v3)
      (,tmp_2 <- 0)
      (,x <- 1))))

;aref
(define (compile-aref v1 v2 x)
  (let ([v2 (encode-const v2)]
        [tmp (new-temp)]
        [tmp_2 (new-temp)]
        [bounds-fail-label (new-fail)]
        [bounds-pass-label (new-pass)]
        [bounds-pass-label-2 (new-pass)])
    `((,tmp_2 <- ,v2);x->tmp_2
      ;(,x >>= 1)
      (,tmp <- (mem ,v1 0))
      (,tmp <<= 1)
      (,tmp += 1)
      (cjump ,tmp_2 < ,tmp  ,bounds-pass-label ,bounds-fail-label)
      ,bounds-pass-label
      ;check if the reference is > 0
      (cjump 0 <= ,tmp_2 ,bounds-pass-label-2 ,bounds-fail-label)
      ,bounds-fail-label
      (eax <- (array-error ,v1 ,v2))
      ,bounds-pass-label-2
      (,tmp_2 >>= 1)
      (,tmp_2 *= 4)
      (,tmp_2 += ,v1)
      (,x <- (mem ,tmp_2 4))
      (,tmp_2 <- 0))))
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
      (append `((eax <- (allocate ,(encode-const (length args)) 0)))
              (make-mem-assigns 'eax args)
              `((,var_x <- eax))) check_tail)]
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
    [(list 'make-closure args ...);;same as new-turple
     (add-return-call
      (append `((eax <- (allocate ,(encode-const (length args)) 0)));3 -> 0
              (make-mem-assigns 'eax args)
              `((,var_x <- eax))) check_tail)]
    [(list 'closure-proc v)
     (add-return-call
       `((,var_x <- (mem ,v 4))) check_tail)];(compile-aref v 0 var_x)
    [(list 'closure-vars v)
     (add-return-call 
       `((,var_x <- (mem ,v 8))) check_tail)];(compile-aref v 1 var_x)
    [(list (? symbol? f) args ...)
     (if (equal? check_tail #t)
         (append (assign-regs args);assign-regs also check if the register already has encoded value
                 `((tail-call ,f)))
         (append (assign-regs args)
                 `((call ,f))
                 `((,var_x <- eax))))]
    ;;constant?
    [_;(list (? symbol? L2_d))
     (add-return-call
      `((eax <- ,(encode-const L2_d))) check_tail)]))

(module+ test
  (test (compile-d '(+ 2 'a) 't #t) '((t <- 5) (t += 'a) (t -= 1) (eax <- t) (eax <- eax) (return)))
  (test (compile-d '(:f var1 var2) 'var #t) '((ecx <- var1) (edx <- var2) (tail-call :f)))
  (test (compile-d '(:f var1 var2 var3) 'var #f) '((ecx <- var1) (edx <- var2) (eax <- var3) (call :f) (var <- eax)))
  ;(test (append `((symbol <- v1)) `((sd <- 5))) '((symbol <- v1) (sd <- 5))) 
  (test (compile-biop '+ 'a 3 '_lei) '((_lei <- a) (_lei += 7) (_lei -= 1) (eax <- _lei)))
  (test (compile-biop '< 'a 'b '_lei) '((_lei <- a < b) (_lei <<= 1) (_lei += 1)))
  (test (encode-const 5) 11)) 

;; compile L3-e
(define (compile-e L3_e)
  (match L3_e
    [`(let ((,x ,L3_d)) ,L3_e_2)
     (compile-let x L3_d L3_e_2)]
    [`(if ,v ,e1 ,e2)
     (compile-if v e1 e2)]
    [_ 
     (compile-d L3_e 'eax #t)]))

; compile let in L3-e
(define (compile-let var L3_d L3_e)
  (cond [(number? L3_d)
         (append `((,var <- ,(encode-const L3_d))) (compile-e L3_e))]
        [(symbol? L3_d)
         (append ;`((,L3_d *= 2) (,L3_d += 1))
          `((,var <- ,L3_d)) (compile-e L3_e))]
        [else (append (compile-d L3_d var #f) (compile-e L3_e))]))

;compile if in L3-e
(define (compile-if v e1 e2)
  (let ([then (new-label)]
        [else (new-label)])
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
  (test (compile-let 'x 3 '(print 3)) '((x <- 7) (eax <- (print 7)) (eax <- eax) (return))))
