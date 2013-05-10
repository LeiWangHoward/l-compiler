#lang plai
;; define label check:
(define (label? s)
  (if (symbol? s)
      (equal? (string-ref (symbol->string s) 0) #\:)
      '#f))

;; define error message
(define (error-message inst)
  (if (symbol? (first inst))
      (string-append "could not register allocate " (symbol->string (first inst)))
      "could not register allocate main"))
;; define all assigned check
#|(define (all-assigned? var_lst pre)
  (let ([all_assign '#f]
        [var_flat (flatten var_lst)]
        [pre_str (symbol->string pre)])
    (map (λ (var)
           (when (not (equal? (substring (symbol->string var) 0 2) pre_str))
             (set! all_assign '#t))
           var_flat))))|#
;;sort a list of instructions
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