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


(define (pred? a)
  (if (symbol? a)
      (or (equal? a 'number?)
          (equal? a 'a?))
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

;;to replace *free* var in e1(&e2) of the letrec
(define (replace-free letrec_e x)
  (match letrec_e
    [`(let ((,var ,e1)) ,e2) 
     `(let ((,var ,(replace-free e1 x))) ,(replace-free e2 x))]
    [`(if ,cond ,then ,else)
     `(if ,(replace-free cond x)
          ,(replace-free then x)
          ,(replace-free else x))]
    [`(begin ,e1 ,e2)
     `(begin ,(replace-free e1 x)
             ,(replace-free e2 x))]
    [`(make-closure ,(? label?) (new-tuple ,args ...))
     letrec_e]
    [`(new-tuple ,args ...)
     letrec_e]
    [(? number? num)
     num]
    [(? symbol? x1)
     (if (equal? x x1)
         `(aref ,x1 0)
         x1)]
    [`(,args ...)
     (map (λ (arg)
            (replace-free arg x))
          args)]))

(module+ test 
  (test (replace-free '(+ a b) 'a) '(+ (aref a 0) b))
  (test (replace-free '(make-closure :f (new-tuple a b c)) 'b) 
        '(make-closure :f (new-tuple a b c))))
;; new name replace function e.g x -> s0 
(define (replace sexp tar_var s_var)
  (cond
    [(equal? tar_var sexp) s_var]
    [(pair? sexp) (cons (replace (car sexp) tar_var s_var)
                        (replace (cdr sexp) tar_var s_var))]
    [else sexp]))

(module+ test 
  (test (replace '(+ 1 (+ 2 x)) 'x '(aref x 0)) '(+ 1 (+ 2 (aref x 0))))) 