#lang plai
(require "register.rkt")
(require "tools.rkt")
(require data/queue)
;brute force 
(define (brute-force-color l2_graph color_lst)
  ;first we update the available color list by cutting off unaviliable colors
  ;if there exist >= one var have 0 color availiable, we return #f 
  (let* ([pre_color 
          (map (λ (color_element)
                 (let ([in_use_reg (cdr (assoc (first color_element) l2_graph))])
                   ;(displayln in_use_reg);test
                   (remove* in_use_reg color_element)))
               color_lst)]
         [interfere_lst
          (map (λ (var_element)
                 (remove* register_lst (assoc (first var_element) l2_graph)))
               color_lst)]
         [post_color pre_color])
    ;(displayln post_color)
    ;(displayln interfere_lst)
    (do ([color_iter post_color (cdr color_iter)])
      ((or (equal? color_iter (list)) (equal? post_color #f)) post_color)
      (let* ([current_node (car color_iter)]
             [node_head (first current_node)])
        (if (>= (length (assoc node_head post_color)) 2); still valid
            (let* ([color_assign (second (assoc node_head post_color))]); pick the "first" color
              (set! post_color 
                    (map (λ (node_edges)
                           (let ([graph_node (assoc (first node_edges) l2_graph)]) 
                             (cond [(member node_head (rest graph_node))
                                    (remove color_assign node_edges)]
                                   [(equal? (first node_edges) node_head)
                                    (list node_head color_assign)]
                                   [else node_edges])))
                         post_color)))
            (set! post_color #f))))))

;minimum remaning value (MRV)
(define (MRV-color l2_graph color_lst)
  (let* ([pre_color 
          (map (λ (var_element)
                 (let ([in_use_reg 
                        (cdr (assoc (first var_element) l2_graph))])
                   (remove* in_use_reg var_element)))
               color_lst)]
         [interfere_var_lst
          (map (λ (var_element)
                 (remove* register_lst 
                          (assoc (first var_element) l2_graph)))
               color_lst)]
         [color_queue (make-queue)])
    (for ([count_end (range (length color_lst))])
      #:break (equal? pre_color #f)  
      (let* ([sorted_lst (sort-by-length pre_color)]
             [mrv_var (first sorted_lst)]
             [var_name (first mrv_var)])
        ;(displayln sorted_lst)
        (if (>= (length mrv_var) 2)
            (let ([var_assign (second mrv_var)])
              (set! pre_color
                    (for/list ([var (cdr sorted_lst)])
                      (if (member var_name (assoc (first var) interfere_var_lst))
                          (remove var_assign var)
                          var)))
              (enqueue! color_queue (list var_name var_assign)))
            (set! pre_color #f))))
    (if (= (queue-length color_queue) (length color_lst))
        (queue->list color_queue)
        #f)))                