#lang plai
(require data/queue)
(require racket/match)
(require racket/set)
(require "register.rkt")
(require "tools.rkt")
;;define enqueue var and register (except esp ebp)
(define (enqueue-var-reg que var-lst)
  (enqueue! que 
            (for/list ([var var-lst]
                       #:when (not (or (number? var)
                                       (equal? var 'esp)
                                       (equal? var 'ebp)
                                       (label? var))))
              var)))
(module+ test
  (test (begin (define a (make-queue)) (enqueue-var-reg a '(1 3 5 esp ebp)) (dequeue! a)) '())
  (test (begin (define a (make-queue)) (enqueue-var-reg a '()) (dequeue! a)) '())
  )   

;;define number/symbol check, return list of symbols
(define (var-lst lhs rhs)
  (cond [(and (number? lhs) (number? rhs))
         (list)]
        [(and (symbol? lhs) (number? rhs))
         (list lhs)]
        [(and (number? lhs) (symbol? rhs))
         (list rhs)]
        [else ; both l and r are symbol
         (list lhs rhs)]))
;; function to get gen & kill list
(define (gen-kill-queue sexp gen kill)
  (match sexp
    [(list (list 'mem ptr num) '<- src)
     (begin
       (enqueue-var-reg gen (list ptr num src))
       (enqueue! kill '()))]
    
    [(list dst '<- (list 'mem ptr num))
     (begin
       (enqueue-var-reg gen (list ptr num))
       (enqueue-var-reg kill (list dst)))];x86 won't kill ptr, no value assigned
    ;;return
    [(list 'return)
     (begin (enqueue! gen (append l_result callee_save));;gen: result and callee_save
            (enqueue! kill '()))]
    ;;call
    [(list 'call s)
     (begin (enqueue-var-reg gen (append l_args (list s)))
            (enqueue! kill (append caller_save l_result)))];;new version
    ;;tail-call
    [(list 'tail-call s)
     (begin (enqueue-var-reg gen (append l_args callee_save (list s)))) 
     (enqueue! kill '())]
    ;;print
    [(list 'eax '<- (list 'print s))
     (begin (enqueue-var-reg gen (list s))
            (enqueue! kill x86_caller_save))]
    ;;allocate, x86 caller save kill!
    [(list 'eax '<- (list 'allocate v0 v1))
     (begin
       (enqueue! gen (var-lst v0 v1))
       (enqueue! kill x86_caller_save))]
    ;;array error
    [(list dst '<- (list 'array-error s0 s1))
     (begin
       (enqueue! gen (var-lst s0 s1))
       (enqueue! kill (list)))]
    ;; cmp operators
    [(list dst '<- l op r)
     (begin 
       (enqueue! gen (var-lst l r))
       (enqueue! kill (list dst)))]
    ;; match cjump operator
    [(list 'cjump l op r _ _)
     (begin
       (enqueue! gen (var-lst l r))
       (enqueue! kill (list)))]; kill l
    ;; two basic situations, spill var without further operation
    [(list dst '<- src)
     (begin (enqueue-var-reg gen (list src))
            (enqueue! kill (list dst)))]
    ;; operators += -= *= &= >>= <<=
    [(list l op r)
     (begin (enqueue-var-reg gen (list l r))
            (enqueue-var-reg kill (list l)))]
    ;; goto, :label etc
    [_ 
     (begin (enqueue! gen '())
            (enqueue! kill '()))]))

;;liveness calculation and check
(define (liveness gen kill out)
  (let ([set_gen (list->set gen)]
        [set_kill (list->set kill)]
        [set_out (list->set out)])
    (set->list (set-union set_gen
                          (set-subtract set_out set_kill)))))

;to calculate and assign gen kill to corresponding gen, kill list
(define (gen-kill-cal inst)
  (let ([gen_que (make-queue)]
        [kill_que (make-queue)])
    (map (lambda (sub_exp)
           (gen-kill-queue sub_exp gen_que kill_que))
         inst)
    (values (queue->list gen_que)
            (queue->list kill_que))))


;;calculate out based on in and gen(instruction)
(define (out-cal inst in)
  (let ([normal_succ (append (cdr in) (list (list)))])
    (for/list ([in_iter in]
               [inst_iter inst]
               [succ normal_succ])
      (match inst_iter
        [(or (list 'return)
             (list 'eax '<- (list 'array-error _ _))
             (list 'tail-call _))
         (list)];;no successor
        [(list 'goto g)
         (let ([tail_len (length (member g inst))])
           (list-ref in (- (length in) tail_len)))]
        [(list 'cjump l op r t_jmp f_jmp)
         (let* ([tail_len_1 (length (member t_jmp inst))]
                [out_ele_1 (list-ref in (- (length in) tail_len_1))]
                [tail_len_2 (length (member f_jmp inst))]
                [out_ele_2 (list-ref in (- (length in) tail_len_2))])
           (remove-duplicates (append out_ele_1 out_ele_2)))]
        [_ ; normal condition, sucessor is next instruction
         succ]))))
;(test (out-cal '((eax <- 3) (y <- 4) (eax <- eax)) '(() () (eax))) '(()(eax)()))
;;init and then calculate in and out list
(define (in-out-cal inst gen_lst kill_lst)
  (let* ([in_lst gen_lst];init in_lst as gen lst
         [out_lst (sort-lst (out-cal inst in_lst))]
         [in_lst_temp (list)])
    (do ([unchange #f])
      ((equal? unchange #t)
       (values in_lst out_lst))
      (set! in_lst_temp (map (lambda (gen kill out)
                               (liveness gen kill out))
                             gen_lst kill_lst out_lst))
      (set! in_lst_temp (sort-lst in_lst_temp))
      (set! unchange (equal? in_lst in_lst_temp))
      (when (equal? unchange #f)
        (set! in_lst in_lst_temp)
        (set! out_lst (sort-lst (out-cal inst in_lst)))))))

;;calculate the live range of variables include calee save,
;as well as number of time accessed
(define (live-range-access gen_lst kill_lst var_lst)
  (let ([var_lst_new var_lst])
    ;(when (equal? main_judge #f) 
    (set! var_lst_new (append var_lst_new (list (list 'esi) (list 'edi))))
    (map (λ (single_var)
           (live-range-access-cal gen_lst kill_lst single_var))
         var_lst_new)))

(define (live-range-access-cal gen_lst kill_lst var)
  (let ([var_name (first var)];;flattern the list
        [current_start 0]
        [range_sum 0]
        [access_time 0]
        [return_tail_call #f]
        [index_lst (range (length gen_lst))])
    (map (λ (single_gen single_kill index)
           (cond [(and (member var_name single_gen) (member var_name single_kill))
                  (begin (set! range_sum 
                               (+ range_sum (- index current_start)))
                         (set! current_start index)
                         (set! access_time (add1 access_time)))] 
                 [(member var_name single_gen)
                  (begin (set! range_sum 
                               (+ range_sum (- index current_start)))
                         (set! access_time (add1 access_time)))] 
                 [(member var_name single_kill)
                  (begin (set! current_start index)
                         (set! range_sum 
                               (add1 range_sum));live~
                         (set! access_time (add1 access_time)))]))
         gen_lst kill_lst index_lst)
    (when (or (= range_sum 0) (> range_sum (length gen_lst))); handle the esi edi
      (set! range_sum (length gen_lst)))
    (append var (list range_sum access_time))))