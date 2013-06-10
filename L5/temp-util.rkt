#lang plai
;; define temp, label count and name
(define var_count -1)

(define (count-one)
  (begin
    (set! var_count (add1 var_count))
    var_count))

(define (back-one)
  (set! var_count (sub1 var_count)))

(define (back-n n)
  (set! var_count (- var_count n)))

(define (reset-count)
  (set! var_count -1))

(define var-pre "var_")
(define app-pre "new_app")

(define (fresh-app)
  (string->symbol
   (format "~a~a" app-pre (count-one))))

(define (fresh-var var)
  (string->symbol
   (format "~a~a~a" var-pre var (count-one))))

(define (line-filter str)
  (string->symbol
   (string-replace (symbol->string str) "-" "_")))
;;used for "postfix" new variables in let and passing variables in λ 
(define (level-var var)
  (string->symbol
   (format "~a~a~a" var '_ 1)))

(define (label-it var)
  (string->symbol
   (format ":~a" (symbol->string var))))