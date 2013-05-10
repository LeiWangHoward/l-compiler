#lang plai
(require data/queue)
(require racket/match)
(require racket/set)
(require "register.rkt")
(require "tools.rkt")
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
       (if (or (equal? ptr 'ebp) (equal? ptr 'esp))
           (if (symbol? src);special case, ignore 'ebp 'esp
               (enqueue! gen (list src))
               (enqueue! gen (list)))
           (if (symbol? src)
               (enqueue! gen (list ptr src))
               (enqueue! gen (list ptr))))
       (enqueue! kill (list)))]
    
    [(list dst '<- (list 'mem ptr num))
     (if (or (equal? ptr 'ebp) (equal? ptr 'esp));special case, ignore 'ebp 'esp
         (begin (enqueue! gen (list))
                (enqueue! kill (list dst)))
         (begin (enqueue! gen (list ptr))
                (enqueue! kill (list dst))))];x86 won't kill ptr, no value assigned
    ;;return
    [(list 'return)
     (begin (enqueue! gen (append l_result callee_save));;gen: result and callee_save
            (enqueue! kill (list)))]
    ;;call
    [(list 'call s)
     (begin (if (label? s)
                (enqueue! gen l_args); no s
                (enqueue! gen (append l_args (list s))))
            (enqueue! kill (append caller_save l_result)))];;new version
    ;;tail-call
    [(list 'tail-call s)
     (begin (if (label? s);label
                (enqueue! gen (append l_args callee_save)) ; no s
                (enqueue! gen (append l_args callee_save (list s)))) 
            (enqueue! kill (list)))]
    ;;print
    [(list 'eax '<- (list 'print s))
     (begin (if (symbol? s)
                (enqueue! gen (list s)) 
                (enqueue! gen (list)))
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
     (begin (if (or (number? src)
                    (equal? src 'ebp) 
                    (equal? src 'esp)
                    (label? src));label
                (enqueue! gen (list))
                (enqueue! gen (list src)))
            (enqueue! kill (list dst)))]
    ;; operators += -= *= &= >>= <<=
    [(list l op r)
     (cond [(or (equal? l 'ebp)
                (equal? l 'esp))
            (begin (if (symbol? r)
                       (enqueue! gen (list r))
                       (enqueue! gen (list)))
                   (enqueue! kill (list)))]
           [(and (symbol? l) (number? r))
            (begin (enqueue! gen (list l))
                   (enqueue! kill (list l)))] 
           [else ; both l, r are symbol
            (begin (enqueue! gen (list l r))
                   (enqueue! kill (list l)))])]
    ;; goto, :label etc
    [_;else 
     (begin (enqueue! gen (list))
            (enqueue! kill (list)))]))

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
  (let ([len (length in)]
        [out_que (make-queue)])
    (do ([in_iter in (cdr in_iter)]
         [inst_iter inst (cdr inst_iter)])
      ((equal? len (queue-length out_que))
       (queue->list out_que))
      (match (car inst_iter)
        [(or (list 'return)
             (list 'eax '<- (list 'array-error _ _))
             (list 'tail-call _))
         (enqueue! out_que (list))];;no successor
        [(list 'goto g)
         (let* ([tail_len (length (member g inst))]
                [out_ele (list-ref in (- (length in) tail_len))])
           (enqueue! out_que out_ele))]
        [(list 'cjump l op r t_jmp f_jmp)
         (let* ([tail_len_1 (length (member t_jmp inst))]
                [out_ele_1 (list-ref in (- (length in) tail_len_1))]
                [tail_len_2 (length (member f_jmp inst))]
                [out_ele_2 (list-ref in (- (length in) tail_len_2))])
           (enqueue! out_que (set->list (list->set (append out_ele_1 out_ele_2)))))]
        [else ; normal condition, sucessor is next instruction
         (if (equal? 1 (length in_iter))
             (enqueue! out_que (list))
             (enqueue! out_que (second in_iter)))]))))

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