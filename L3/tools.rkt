#lang plai
;; define temp, label count and name
(define var_count -1)

(define (count-one)
  (begin
    (set! var_count (add1 var_count))
    var_count))

(define tmp-pre '_lei)
(define label-pre ':_lablei)
(define bounds-pass-label-pre ':_lei_good)
(define bounds-fail-label-pre ':_lei_bad)

(define (new-temp)
  (string->symbol (string-append
                   (symbol->string tmp-pre)
                   (number->string (count-one)))))
(define (new-label)
  (string->symbol (string-append
                   (symbol->string label-pre)
                   (number->string (count-one)))))
(define (new-pass)
  (string->symbol (string-append
                   (symbol->string bounds-pass-label-pre)
                   (number->string (count-one)))))

(define (new-fail)
  (string->symbol (string-append
                   (symbol->string bounds-fail-label-pre)
                   (number->string (count-one)))))
(module+ test
  (test (new-temp) '_lei0)
  (test (new-label) ':_lablei1))
;; define variable checks:

(define (label? s)
  (if (symbol? s)
      (equal? (string-ref (symbol->string s) 0) #\:)
      #f))

(define (biop? s)
  (if (symbol? s)
      (or (equal? s '+)
          (equal? s '-)
          (equal? s '*)
          (equal? s '<)
          (equal? s '<=)
          (equal? s '=))
      #f))

(define (L3-var? s)
  (if (symbol? s)
      (not (equal? (string-ref (symbol->string s) 0) #\:))
      #f))

(define (pred? a)
  (if (symbol? a)
      (or (equal? a 'number?)
          (equal? a 'a?))
      #f))
;;define encode function
(define (encode-const var)
  (if (number? var)
      (+ 1 (* var 2))
      var))

(module+ test
  (test (encode-const 'a) 'a)
  (test (encode-const 5) 11)) 

;;define assign register, and register assign
(define register-lst (list 'ecx 'edx 'eax))

;we have to make sure always use eax last
(define (assign-regs arg-lst)
  (let ([len (length arg-lst)])
    (cond [(= 1 len)
           `((eax <- ,(car arg-lst)))]
          [(= 2 len)
           (for/list ([reg (cdr register-lst)]
                      [args arg-lst])
             `(,reg <- ,args))]
          [(= 3 len) 
           (for/list ([reg register-lst]
                      [args arg-lst])
             `(,reg <- ,args))])))

(define (reg-assign arg-lst)
  (let ([len (length arg-lst)])
    (cond [(= 1 len)
           `((,(car arg-lst) <- eax))]
          [(= 2 len)
           (for/list ([reg (cdr register-lst)]
                      [args arg-lst])
             `(,args <- reg))]
          [(= 3 len) 
           (for/list ([reg register-lst]
                      [args arg-lst])
             `(,args <- reg))])))

(module+ test ;(reg-assign (list 'a 'b 'c))
  (test (assign-regs '(1 's 3)) '((ecx <- 1) (edx <- 's) (eax <- 3))))

;;define memory assign and function var assign
;; make-mem-assigns
(define (make-mem-assigns symbol args)
  (let ([len (length args)])
    (for/list ([i (in-range len)]
               [arg args])
      `((mem ,symbol ,(+ (* i 4) 4))
        <- ,(encode-const arg)))))

(define (add-return-call sexp check)
  (if (equal? #t check)
      (append sexp `((return)))
      sexp))
(module+ test
  (test (add-return-call '((a <- 4)) #t) '((a <- 4) (return))))
;; make-fae-assigns : listof v -> listof d

(define (sort-lst lst)
  (map (lambda (sub_lst)
         (sort sub_lst
               (lambda (x y) (string<? (symbol->string x) (symbol->string y)))))
       lst))

; function to sort a list based on each element's "head" 
(define (sort-single-lst lst)  
  (sort lst
        (lambda (x y) (string<? (symbol->string (car x)) (symbol->string (car y))))))

;; new name replace function e.g x -> s0 
(define (name-replace sexp tar_var s_var)
  (cond
    [(equal? tar_var sexp) s_var]
    [(pair? sexp) (cons (name-replace (car sexp) tar_var s_var)
                        (name-replace (cdr sexp) tar_var s_var))]
    [else sexp]))

;; replace all old_name with new_name
(define (replace-all inst tar_var s_var)
  (map (λ (single_inst)
         (name-replace single_inst tar_var s_var))
       inst))