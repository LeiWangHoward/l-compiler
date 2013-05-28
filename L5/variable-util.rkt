#lang plai
(define prim_set (list 'new-array 'aref 'aset 'alen 'print))

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

(define (var? s)
  (if (symbol? s)
      (let* ([str (symbol->string s)]
             [tail (sub1 (string-length str))])
        (and (not (equal? (string-ref str 0) #\:))
             (not (equal? (string-ref str tail) #\?))
             (not (prim? s))))
      #f))

(define (prim? s)
  (if (symbol? s)
      (cond 
        [(biop? s) #t]
        [(pred? s) #t]
        [(member s prim_set) #t]
        [else #f])
      #f))

;;test 
(module+ test
  (test (var? 'aa) #t)
  (test (var? 'sdsd?) #f)
  (test (var? '+) #f)
  (test (var? 'print) #f)
  (test (prim? '+) #t)
  (test (prim? 'print) #t)
  (test (prim? 'number?) #t))

(define (pred? a)
  (if (symbol? a)
      (or (equal? a 'number?)
          (equal? a 'a?))
      #f))

;; new name replace function e.g x -> s0 
(define (replace sexp tar_var s_var)
  (cond
    [(equal? tar_var sexp) s_var]
    [(pair? sexp) (cons (replace (car sexp) tar_var s_var)
                        (replace (cdr sexp) tar_var s_var))]
    [else sexp]))

(module+ test 
  (test (replace '(+ 1 (+ 2 x)) 'x '(aref x 0)) '(+ 1 (+ 2 (aref x 0))))) 