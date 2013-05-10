#lang plai
;;problems: spill esi edi: redundant calculations, in other words, liveness calculation is wrong!
(require racket/file)
(require data/queue)
(require "tools.rkt")
(require "register.rkt")
(require "spill.rkt")
(require "liveness.rkt")
(require "graph.rkt")
(require "coloring.rkt")
; file input output
(define filename (command-line #:args (filename) filename))
;(define filename "../322-interps/tests/41/2-test/8.L2")
(define inst_in (call-with-input-file filename read))
(define L1_inst (make-queue))

; replace the l2 expression to l1(var->registers)
(define (l2-to-l1 color_lst l2_exp)
  (for ([color color_lst])
    (set! l2_exp (map (λ (single_exp)
                        (let ([l2_var (first color)]
                              [l1_reg (second color)])
                          (name-replace single_exp l2_var l1_reg)))
                      l2_exp)))
  l2_exp)
;(test 
; (l2-to-l1 '((cow grass) (me eax)) (list (list 'cow 'is 'me) (list 'cow 'is 'you)))
; (list (list 'grass 'is 'eax) (list 'grass 'is 'you)))
;main function
(define (main-func)
  (let ([cannot_allocate #f]
        [error_message (string)])
    ;;list bounch of "global variables"
    (map (λ (inst)
           ;;After each spill, we recalculate everything
           (if (empty? inst) ; might be empty instruction!
               (enqueue! L1_inst '())
             (do ([inst_local inst]
                  [stack_num 0];;stack pointer starts from -4
                  [valid_inst #f]
                  [var_num 0])
               ((or valid_inst cannot_allocate) (enqueue! L1_inst inst_local));end loop
               (let*-values ([(gen_lst kill_lst) (gen-kill-cal inst_local)]
                             [(in_lst out_lst) (in-out-cal inst_local gen_lst kill_lst)]
                             [(var_lst) (get-var-lst gen_lst kill_lst)];get variable list that needs to be colored
                             [(l2_graph node_colors) (init-graph-color register_graph var_lst)]
                             [(l2_graph) (graph-construct l2_graph inst_local in_lst out_lst kill_lst)]
                             [(node_colors) (MRV-color l2_graph node_colors)]);(brute-force-color l2_graph node_colors)])
                 (when (equal? var_num 0) 
                   (set! var_num (length var_lst)))
                 ;(displayln var_num)
                 ;(displayln inst_local) ;;test
                 ;(displayln (list (cons 'gen gen_lst) (cons 'kill kill_lst)));test liveness gen & kill
                 ;(displayln (list (cons 'in in_lst) (cons 'out out_lst)));test liveness in & out 
                 ;(displayln l2_graph);test graph
                 ;(displayln node_colors);test graph
                 ;(displayln stack_num); test
                 (if (equal? node_colors #f)
                     (if (> (/ stack_num -4) (+ var_num 2));(+ (length var_lst) 2));(all-assigned? var_lst 's_)
                         (begin (set! error_message (error-message inst_local)) 
                                (set! inst_local (error-message inst_local))
                                (set! cannot_allocate #t)) 
                         (let* ([live_range_access (live-range-access gen_lst kill_lst var_lst)]
                                [spill_var (choose-spill live_range_access)])
                           ;(displayln live_range_access);test
                           ;(displayln spill_var);test
                           (if (l_register? spill_var) 
                               (set! inst_local (spill-callee inst_local spill_var 'temp_))
                               (begin       
                                 (set! stack_num (- stack_num 4))
                                 (set! inst_local (spill-result inst_local spill_var stack_num 's_))))))
                     (begin 
                       (set! valid_inst #t)
                       ;(displayln inst_local);oroginal, test
                       (set! inst_local (l2-to-l1 node_colors inst_local))
                       (set! var_num 0)
                       ;(displayln inst_local);test
                       ;(displayln stack_num);test
                       (when (< stack_num 0)
                         (set! inst_local (esp-adjust inst_local stack_num)))))))))
         inst_in)
    (if (equal? cannot_allocate #t)
        error_message
        (display (queue->list L1_inst)))))

(main-func)
;; number for stack pointer, will always be decrease by 4
;;define/contract