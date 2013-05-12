#lang plai
;; define label check:
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