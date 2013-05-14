#lang plai
;; define name check for stupid people
(define L2_key (list 'mem 'goto 'cjump 'call 'tail-call
                      'return 'allocate 'array-error))

;; new name replace function e.g x -> s0 
(define (name-replace sexp tar_var s_var)
  (cond
    [(equal? tar_var sexp) s_var]
    [(pair? sexp) (cons (name-replace (car sexp) tar_var s_var)
                        (name-replace (cdr sexp) tar_var s_var))]
    [else sexp]))
; we will check in two places: new turp and the let's var
(define (new-key-name key)
  (string->symbol (string-append
                   (symbol->string '_new_)
                   (symbol->string key))))
;;key var filter
(define (L2-key-replace l_exp)
  (for/fold ([l_exp l_exp]) 
    ([key (in-list L2_key)])
    (let ([new_name (new-key-name key)])
      (name-replace l_exp key new_name))))
;test
(module+ test
  (test (L2-key-replace '( (let ((goto 1))
                             (let ((cjump 2))
                               (let ((call 3))
                                 (let ((tail-call 4))
                                   (let ((return 5))
                                     (let ((allocate 6))
                                       (let ((array-error 7))
                                         (let ((arr (new-tuple mem goto cjump call tail-call return allocate array-error))
                                               )
                                           (let ((a (print goto)))))))))))))) 'what))

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
      (add1 (* var 2))
      var))

(module+ test
  (test (encode-const 'a) 'a)
  (test (encode-const 5) 11)) 

;;define assign register, and register assign
(define register-lst (list 'ecx 'edx 'eax))

;simple assign-regs and reg-assign
(define (assign-regs arg-lst)
  (for/list ([reg register-lst]
             [args arg-lst])
    `(,reg <- ,(encode-const args))));;add key check

(define (reg-assign arg-lst)
  (for/list ([reg register-lst]
             [args arg-lst])
    `(,args <- ,reg)))

;test
(module+ test 
  (test (assign-regs '(1 's 3)) '((ecx <- 3) (edx <- 's) (eax <- 7))))

;;define memory assign and function var assign
;; make-mem-assigns
(define (make-mem-assigns symbol args)
  (let ([len (length args)])
    (for/list ([i (in-range len)]
               [arg args])
      `((mem ,symbol ,(+ (* i 4) 4))
        <- ,(encode-const arg)))))

;add return to function
(define (add-return-call sexp check)
  (if check
      (append sexp `((return)))
      sexp))

;test
(module+ test
  (test (add-return-call '((print a)) #t) '((print a) (return)))
  (test (add-return-call '((a <- 4)) #t) '((a <- 4) (return))))