#lang plai
;; define temp, label count and name
(define var_count -1)

(define (count-one)
  (begin
    (set! var_count (add1 var_count))
    var_count))

(define var-pre "var_")
(define app-pre "new_app")

(define (fresh-app)
 (string->symbol
   (format "~a~a" app-pre (count-one)))) 
(define (fresh-var var)
  (string->symbol
   (format "~a~a~a" var-pre var (count-one))))

(define (label-it var)
  (string->symbol
   (format ":~a" (symbol->string var))))

(module+ test
  (test (fresh-var 'x) 'var_x0)
  (test (label-it 'a) ':a)
  (test (fresh-app) 'new_app1))
        