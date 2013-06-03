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
(define L1_inst (make-queue))

; replace the l2 expression to l1(var->registers)
(define (l2-to-l1 color_lst l2_exp)
  (for/fold ([l2_exp l2_exp]) 
    ([color (in-list color_lst)])
    (let ([l2_var (first color)]
          [l1_reg (second color)])
      (name-replace l2_exp l2_var l1_reg))))

;main function
(define (compile-L2 inst_in)
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
                 (let ()
                   (define-values (gen_lst kill_lst) (gen-kill-cal inst_local))
                   ;(displayln (format "gen~akill~a" gen_lst kill_lst));test
                   (define-values (in_lst out_lst) (in-out-cal inst_local gen_lst kill_lst))
                   ;(displayln (format "in~aout~a" in_lst out_lst));test
                   (define-values (var_lst) (get-var-lst gen_lst kill_lst));get variable list that needs to be colored
                   ;(displayln (format "var~a" var_lst));test
                   (define-values (l2_graph node_colors) (init-graph-color register_graph var_lst))
                   (set! l2_graph (graph-construct l2_graph inst_local in_lst out_lst kill_lst))
                   (set! node_colors (MRV-color l2_graph node_colors));(brute-force-color l2_graph node_colors)])
                   ;(displayln (format "colors ~a" node_colors));test
                   (when (equal? var_num 0) ;init total number of var
                     (set! var_num (length var_lst)))
                   (if (equal? node_colors #f)
                       (if (> (/ stack_num -4) (+ var_num 2))
                           (begin (set! error_message (error-message inst_local)) 
                                  (set! inst_local (error-message inst_local))
                                  (set! cannot_allocate #t)) 
                           (let* ([live_range_access (live-range-access gen_lst kill_lst var_lst)]
                                  [spill_var (choose-spill live_range_access)])
                             ;(displayln (format "live-range ~a choose-which ~a" live_range_access spill_var));test
                             (if (l_register? spill_var) 
                                 (set! inst_local (spill-callee inst_local spill_var 'temp_))
                                 (begin       
                                   (set! stack_num (- stack_num 4))
                                   (set! inst_local (spill-result inst_local spill_var stack_num 's_))))))
                       (begin 
                         (set! valid_inst #t)
                         (set! inst_local (l2-to-l1 node_colors inst_local))
                         (set! var_num 0)
                         (when (< stack_num 0)
                           (set! inst_local (esp-adjust inst_local stack_num)))))))))
         inst_in)
    (if (equal? cannot_allocate #t)
        error_message
        (queue->list L1_inst))))
;; number for stack pointer, will always be decrease by 4
;;define/contract
#|
(define filename (command-line #:args (filename) filename))
(define exp (call-with-input-file filename read))
(displayln (compile-L2 exp))|#