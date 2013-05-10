#lang plai
(require racket/match)
(require racket/string)
(require data/queue)
(require "tools.rkt")
(require "register.rkt")
;; define a list that has four parts: expression, variable name,
;; offset [multiple of 4], stack name [variable]
;;; now handle name for the stack location
;; number for variable name, e.g s_0, s_1
(define name_num -1)
;; reset the number for variable name
(define (reset_num)
  (set! name_num -1))
;; set a new name, then increase name number by one
(define (new-name prefix)
  (begin
    (set! name_num (add1 name_num))
    (string->symbol (string-append
                     (symbol->string prefix)
                     (number->string name_num)))))
;; reset name number
(define (back-one)
  (set! name_num (sub1 name_num)))

;; choose which one to spill
(define (choose-spill live_range_access)
  (let ([chosen 'none]
        [max 0])
    (map (λ (element)
           (let* ([live_range (second element)]
                  [access_time (third element)]
                  [name (first element)]
                  [possible (- live_range (/ access_time 2))])
             (when (and (> possible max) (not (spilled? name 's_)))
               ;(unless (and (l_register? name) (> access_time 0)) 
                 (begin (set! chosen (first element))
                        (set! max possible)))));) 
         live_range_access)
    chosen))
;; update esp
(define (esp-adjust inst stack_num)
  (let ([label_check (first inst)]
        [end_check (last inst)])
  (if (label? label_check); a function?
      (when (end-inst? end_check)
        (append (list label_check) (list (list 'esp '+= stack_num)) (cdr inst)))
      (append (list (list 'esp '+= stack_num)) inst (list (list 'esp '-= stack_num))))))
     
;; check if instruction is (return) or tail-call
(define (end-inst? inst)
  (if (list? inst)
      (or (equal? (list 'return) inst)
          (equal? 'tail-call (first inst)))
      '#f))
;;spill callee_save
(define (spill-callee inst callee_reg pre);off)
  (let* ([temp_s (new-name pre)];(list 'mem 'ebp off)]directly replace with stack
         [temp_inst (replace-all inst callee_reg temp_s)];replace 'esi 'edi with temp_s
         [label_check (first temp_inst)]
         [new_inst (make-queue)]
         [inst_lst (list)])
    (if (label? label_check); handle first instruction might be function name
        (begin (enqueue! new_inst label_check)
               (enqueue! new_inst (list temp_s '<- callee_reg)))
        (begin (enqueue! new_inst (list temp_s '<- callee_reg))
               (enqueue! new_inst label_check)))
    (map (λ (inst)
           (if (end-inst? inst)
               (begin (enqueue! new_inst (list callee_reg '<- temp_s))
                      (enqueue! new_inst inst))
               (enqueue! new_inst inst)))
         (cdr temp_inst))
    (set! inst_lst (queue->list new_inst))
    (if (end-inst? (last inst_lst))
        inst_lst
        (append inst_lst (list (list callee_reg '<- temp_s))))));register's temp did not count!
;;;now create functions that handle the spill expression substitute
;; helper functions for access/update memory 
(define (access-mem dst offset)
  (list dst '<- (list 'mem 'ebp offset)))

(define (update-mem src offset)
  (list (list 'mem 'ebp offset) '<- src))

;; main substitute function
(define (spill-parse spill_que sexp var_name offset pre)
  (match sexp
    ;; special: allocate, array-error, print(these three might 
    ;; conflict with (dst '<- src) ), call, tail-call, goto
    [(or (list 'eax '<- (? list?))
         (list 'call _)
         (list 'tail-call _)
         (list (? list?) '<- _)) 
     (let* ([new_name (new-name pre)]
            [new_exp  (name-replace sexp var_name new_name)])
       (if (equal? sexp new_exp)
           (begin (back-one) 
                  (enqueue! spill_que sexp))
           (begin (enqueue! spill_que (access-mem new_name offset))
                  (enqueue! spill_que new_exp))))]
    ;;handle special case with 'mem
    [(list dst '<- (list 'mem ptr num))
     (let* ([new_name (new-name pre)]
            [new_exp  (name-replace sexp var_name new_name)])
       (if (equal? dst var_name) 
           (if (equal? ptr var_name)
               (begin (enqueue! spill_que (access-mem new_name offset)) 
                      (enqueue! spill_que new_exp)
                      (enqueue! spill_que (update-mem new_name offset)))
               (begin (enqueue! spill_que new_exp)
                      (enqueue! spill_que (update-mem new_name offset))))
           (if (equal? ptr var_name)
               (begin (enqueue! spill_que (access-mem new_name offset))
                      (enqueue! spill_que new_exp))
               (begin (back-one) 
                      (enqueue! spill_que sexp)))))]
    ;; two basic situations, spill var without further operation
    [(list dst '<- src)
     (when (not (and (equal? src dst)
                     (equal? src var_name)))
         ;special case, empty list
       (cond 
         [(equal? dst var_name) (enqueue! spill_que (update-mem src offset))]
         [(equal? src var_name) (enqueue! spill_que (access-mem dst offset))]      
         [else (enqueue! spill_que sexp)]))]
    ;; match cmp operators
    [(list dst '<- l op r)
     (let* ([new_name (new-name pre)]
            [new_exp  (name-replace sexp var_name new_name)])
       (if (equal? dst var_name)
           (if (or (equal? l var_name)
                   (equal? r var_name))
               (begin (enqueue! spill_que (access-mem new_name offset))
                      (enqueue! spill_que new_exp)
                      (enqueue! spill_que (update-mem new_name offset)))
               (begin (enqueue! spill_que new_exp)
                      (enqueue! spill_que (update-mem new_name offset))))
           (if (or (equal? l var_name)
                   (equal? r var_name))
               (begin (enqueue! spill_que (access-mem new_name offset))
                      (enqueue! spill_que new_exp))
               (begin (back-one) 
                      (enqueue! spill_que sexp)))))]
    ;; match cjump operator
    [(list 'cjump l op r t_jmp f_jmp)
     (let ([new_name (new-name pre)])
       (cond
         [(equal? l var_name)
          (if (equal? r var_name)
              (begin (enqueue! spill_que (access-mem new_name offset))
                     (enqueue! spill_que (list 'cjump new_name op new_name t_jmp f_jmp)))
              (begin (enqueue! spill_que (access-mem new_name offset))
                     (enqueue! spill_que (list 'cjump new_name op r t_jmp f_jmp))))]
         [(equal? r var_name)
          (begin (enqueue! spill_que (access-mem new_name offset))
                 (enqueue! spill_que (list 'cjump l op new_name t_jmp f_jmp)))]
         [else 
          (begin (back-one) 
                 (enqueue! spill_que sexp))]))]
    
    ;; operators += *= etc
    [(list l op r)
     (let* ([new_name (new-name pre)]
            [new_sexp (name-replace sexp var_name new_name)])
       (cond
         [(equal? l var_name)
          (begin (enqueue! spill_que (access-mem new_name offset))
                 (enqueue! spill_que new_sexp)
                 (enqueue! spill_que (update-mem new_name offset)))]
         [(equal? r var_name)
          (begin (enqueue! spill_que (access-mem new_name offset))
                 (enqueue! spill_que new_sexp))]
         [else (begin (back-one) 
                      (enqueue! spill_que sexp))]))]
    
    ;; return, goto, label etc
    [_ (enqueue! spill_que sexp)]))

; here is the tricky part, we add one extra "()" to all the elements
(define (spill-result inst var off pre)
  ;; Grab each instruction and add it to our string
  (let ([spill_que (make-queue)])
    (map (λ (sub_exp)
           (spill-parse spill_que sub_exp var off pre))
         inst)
    (if (queue-empty? spill_que)
        (list)
        (queue->list spill_que))))