#lang plai
;; define name check for stupid people
(define L2_key (list 'mem 'goto 'cjump 'call 'tail-call
                     'return 'allocate 'array-error))

(define L4_key (list 'let 'if 'new-array 'new-tuple 'aref
                     'aset 'alen 'begin 'print 'make-closure 
                     'closure-proc 'closure-vars))

(define (L4-key? key)
  (let [(key_shrink
         (remove* (list 'let 'if 'begin) L4_key))]
    (if (symbol? key)
        (when (member key key_shrink)
          #t)
        #f)))
;; new name replace function e.g x -> s0 
(define (name-replace sexp tar_var s_var)
  (cond
    [(equal? tar_var sexp) s_var]
    [(pair? sexp) (cons (name-replace (car sexp) tar_var s_var)
                        (name-replace (cdr sexp) tar_var s_var))]
    [else sexp]))

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

;;variable name replace in function
(define (new-arg-name args)
  (for/list
      ([arg (in-list args)])
    (if (symbol? arg)
        (string->symbol (string-append
                         (symbol->string arg)
                         "_"
                         (number->string (count-one))))
        arg)))

(define (var-replace l_exp args new_args)
  (for/fold ([l_exp l_exp]) 
    ([arg (in-list args)]
     [new_arg (in-list new_args)])
    (if (symbol? arg)
        (name-replace l_exp arg new_arg)
        l_exp)))

;;filter out quote of d
(define (quote-filter d)
  (if (list? d)
      (cond [(empty? d) ""];will be filter out by displayln
            [(= (length d) 1) (first d)]
            [else d])
      d))

;; define temp, label count and name
(define var_count -1)

(define (count-one)
  (begin
    (set! var_count (add1 var_count))
    var_count))

(define var-pre '_var_)
(define label-pre ':_lab_)

(define (fresh-var)
  (string->symbol (string-append
                   (symbol->string var-pre)
                   (number->string (count-one)))))

(module+ test
  (test (fresh-var) '_var_0))

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

(define (val? s)
  (or (number? s) (symbol? s)))

(define (var? s)
  (if (symbol? s)
      (let* ([str (symbol->string s)]
             [tail (sub1 (string-length str))])
        (and (not (equal? (string-ref str 0) #\:))
             (not (equal? (string-ref str tail) #\?))
             (not (member s L4_key))))
      #f))

;;test 
(module+ test
  (test (var? ':333) #f)
  (test (var? 'aa) #t)
  (test (var? 'sdsd?) #f))

(define (pred? a)
  (if (symbol? a)
      (or (equal? a 'number?)
          (equal? a 'a?))
      #f))
;;define encode function