#lang plai
;; define label check:
(define (label? s)
  (if (symbol? s)
      (equal? (string-ref (symbol->string s) 0) #\:)
      '#f))

;; to check if respill occurs
(define (spilled? s pre)
  (if (symbol? s)
      (let ([name_string (symbol->string s)])
        (if (> (string-length name_string) 2)
          (equal? (substring name_string 0 2) (symbol->string pre))
          #f))
      #f))
;(test (spilled? 's_a 's_) #t)
;;calculate the var number
(define (var-num var_lst)
  (let ([c 0])
    (for ([var var_lst])
      (unless (spilled? (car var) 's_)
        (set! c (add1 c))))
    c))
;(test (spilled? 's_2 's_) #t)
;; define error message
(define (error-message inst)
  (if (symbol? (first inst))
      (format "could not register allocate ~a"(first inst))
      "could not register allocate main"))
;;sort a list of instructions
(define (sort-by-length lst)
  (sort lst
        (lambda (x y) (< (length x) (length y)))))

#|(test (sort-by-length '((a eax ebx ecx edi) (x0 eax ebx ecx edi edx)
                                            (x1 eax ebx ecx edi edx esi) (x2 ebx) (x3 ebx edi esi)))
      '( (x2 ebx) (x3 ebx edi esi) (a eax ebx ecx edi) (x0 eax ebx ecx edi edx)
                  (x1 eax ebx ecx edi edx esi)))|#

;;sort-by-length
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