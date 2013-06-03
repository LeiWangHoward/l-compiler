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

;; define temp, label count and name
(define var_count_L4 -1)

(define (L4-count-one)
  (begin 
    (set! var_count_L4 (add1 var_count_L4))
    var_count_L4))

(define 4-var-pre 'l4_var_)

(define (L4-fresh-var)
  (string->symbol 
   (format "~a~a" 4-var-pre (L4-count-one))))

(module+ test
  (test (L4-fresh-var) 'l4_var_0))

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