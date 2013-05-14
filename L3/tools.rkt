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
(define encode-lst '())

(define (encode-const var)
  (if (number? var)
      (+ 1 (* var 2))
      var))
;;if var, and not encoded, encode it
(define (encode-if-var var)
  (if (L3-var? var)
      (if (member var encode-lst)
          '()  
          (begin 
            (set! encode-lst (append encode-lst `(,var)))
            `((,var <<= 1)
              (,var += 1))))
      '()))
;;add to encode list if it is not there
(define (add-encode var)
  (unless (member var encode-lst) 
    (set! encode-lst (append encode-lst `(,var)))))

(define (remove-encode var) 
  (set! encode-lst (remove var encode-lst)))

(define (decode-if-reg reg var)
  (if (and (member reg register-lst) 
           (or (member var encode-lst)
               (member reg encode-lst)))
      (begin 
        (remove-encode reg) 
        `(;decode var before assign
          (,reg >>= 1)))
      '()))

(module+ test
  (test (encode-const 'a) 'a)
  (test (encode-const 5) 11)) 

;;define assign register, and register assign
(define register-lst (list 'ecx 'edx 'eax))

;simple assign-regs and reg-assign
(define (assign-regs arg-lst)
  (for/list ([reg register-lst]
             [args arg-lst])
    `(,reg <- ,(encode-const args))))

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