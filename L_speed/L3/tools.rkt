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
                                           (let ((a (print goto)))))))))))))) 
        '((let ((_new_goto 1)) 
            (let ((_new_cjump 2))
              (let ((_new_call 3)) 
                (let ((_new_tail-call 4))
                  (let ((_new_return 5))
                    (let ((_new_allocate 6))
                      (let ((_new_array-error 7))
                        (let ((arr (new-tuple _new_mem _new_goto _new_cjump
                                              _new_call _new_tail-call
                                              _new_return _new_allocate
                                              _new_array-error)))
                          (let ((a (print _new_goto)))))))))))))))

;; define temp, label count and name
(define L3_var_count -1)

(define (L3-count-one)
  (begin
    (set! L3_var_count (add1 L3_var_count))
    L3_var_count))

(define tmp-pre '_l3_tmp)
(define label-pre ':_labl3_)
(define bounds-pass-label-pre ':_l3_good)
(define bounds-fail-label-pre ':_l3_bad)

(define (new-temp)
  (string->symbol (format "~a~a"
                          tmp-pre
                          (L3-count-one))))
(define (new-label)
  (string->symbol (format "~a~a"
                          label-pre
                          (L3-count-one))))
(define (new-pass)
  (string->symbol (format "~a~a"
                          bounds-pass-label-pre
                          (L3-count-one))))

(define (new-fail)
  (string->symbol (format "~a~a"
                          bounds-fail-label-pre
                          (L3-count-one))))
(module+ test
  (test (new-temp) '_l3_tmp0)
  (test (new-label) ':_labl3_1))
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
      `(,@sexp (return))
      sexp))

;test
(module+ test
  (test (add-return-call '((print a)) #t) '((print a) (return)))
  (test (add-return-call '((a <- 4)) #t) '((a <- 4) (return))))