#lang plai
(require data/queue)
(require "tools.rkt")
(require "register.rkt")

(define color_lst (list))

;;functions for graph and color map initialization
; graph representation, first: node; rest: its neighbors  
(define register_graph
  (list (list 'eax 'ebx 'ecx 'edi 'edx 'esi)
        (list 'ebx 'eax 'ecx 'edi 'edx 'esi)
        (list 'ecx 'eax 'ebx 'edi 'edx 'esi)
        (list 'edi 'eax 'ebx 'ecx 'edx 'esi)
        (list 'edx 'eax 'ebx 'ecx 'edi 'esi)
        (list 'esi 'eax 'ebx 'ecx 'edi 'edx)))

; create and fill in variable list
(define (get-var-lst l2_gen l2_kill)
  (let ([var_lst (make-queue)])
    (map (λ (gen_sub)
           (map (λ (gen_ele)
                  (unless (l_register? gen_ele) 
                    (enqueue! var_lst (list gen_ele))))
                gen_sub))
         l2_gen)
    (map (λ (kill_sub)
           (map (λ (kill_ele)
                  (unless (l_register? kill_ele);check if it is not register 
                    (enqueue! var_lst (list kill_ele))))
                kill_sub))
         l2_kill)
    (remove-duplicates (queue->list var_lst))))

; init l2_graph and color map with variables. Also, make "all color set" available to each variable
(define (init-graph-color l2_graph var_lst)
  (values (sort-single-lst (append l2_graph var_lst))
          (sort-single-lst (map (λ (l2_var)
                                  (append l2_var register_lst))
                                var_lst))))

; function handle edge add 
(define (edge-add edge_lst interfere_lst)
  (let ([node (car edge_lst)]
        [interfere_set (list->set interfere_lst)]
        [edge_set (list->set edge_lst)]) ;first element of edge_lst is the node 
    (when (set-member? interfere_set node)
      (set! edge_lst
            (set->list (set-union edge_set interfere_set))))
    ;keep the node "index name" in front
    (append (list node) (remove node edge_lst))))

; to check the instruction if kill and out corredpond to (y <- x)
(define (kill-out-exception? inst kill out)
  (if (symbol? inst) #f ;when instruction is not a list
      (if (and (= (length inst) 3)
               (equal? (second inst) '<-))
          (let ([lhs (first inst)]
                [rhs (third inst)])
            (and (member lhs kill)
                 (or (member rhs out)
                     (member rhs kill))));assign to itself
          #f)))
; to check the out set
(define (out-exception? inst out)
  (if (symbol? inst) #f
      (if (and (= (length inst) 3)
               (equal? (second inst) '<-))
          (let ([lhs (first inst)]
                [rhs (third inst)])
            (and (member lhs out)
                 (member rhs out)))
          #f)))
;;Function update graph by edge-check-add function
(define (graph-construct l2_graph inst_in l2_in l2_out l2_kill)
  ;1) we first handle the 1st instruction of l2_in set
  (let ([new_graph
         (map (λ (node_edges)
                (edge-add node_edges (first l2_in)))
              l2_graph)]
        [kill_out_lst (list)])
    ;2) we handle the combinations of out list and kill list
    (for ([out_single l2_out]
          [kill_single l2_kill]
          [inst_single inst_in])
      (let* ([l_interfere (list)]
             [r_interfere (list)]
             [kill_out_lst (remove-duplicates (append kill_single out_single))])
        (cond [(out-exception? inst_single out_single)
               (begin (set! l_interfere (remove (first inst_single) kill_out_lst))
                      (set! r_interfere (remove (third inst_single) kill_out_lst)))] 
              [(kill-out-exception? inst_single kill_single out_single)
               (begin (set! l_interfere (remove (third inst_single) kill_out_lst))
                      (set! r_interfere (remove* kill_single out_single)))]
              [else 
               (begin (set! l_interfere kill_out_lst)
                      (set! r_interfere (list)))])
        ;(displayln (cons l_interfere r_interfere));test
        ;now we update the new_graph
        (unless (<= (length l_interfere) 1)
          (set! new_graph (map (λ (node_edges)
                                 (edge-add node_edges l_interfere))
                               new_graph)))
        (unless (<= (length r_interfere) 1)
          (set! new_graph (map (λ (node_edges)
                                 (edge-add node_edges r_interfere))
                               new_graph)))))
    ;3) we add extra edges for (cx <- s cmp s) and (x sop=sx)
    ; a. (cx <- s cmp s), cx have edges edi esi
    ; b. x sop= sx, ex hage edges to everything except ecx
    (for ([inst_iter inst_in])
      (set! new_graph (map (λ (node_edges)
                             (match inst_iter
                               [(list dst '<- _ cmp _)
                                (if (equal? dst (first node_edges))
                                    (append node_edges (list 'esi 'edi))
                                    node_edges)]
                               [(or (list _ '<<= _) (list _ '>>= _))
                                (if (equal? (third inst_iter) (first node_edges))
                                    (append node_edges (list 'eax 'ebx 'edi 'edx 'esi))
                                    node_edges)]
                               [else node_edges]))
                           new_graph)))
    ;update graph
    (set! new_graph (graph-update new_graph))
    (graph-clean new_graph)))
; function to update the whole graph
(define (graph-update graph)
  (do ([graph_iter graph (cdr graph_iter)])
    ((equal? graph_iter (list)) graph)
    (let ([graph_single_iter (car graph_iter)])
      (set! graph
            (map (λ (node_edges)
                   (if (member (first node_edges) graph_single_iter)
                       (append node_edges (list (first graph_single_iter)))
                       node_edges));update the original graph
                 graph)))))
;function to finalize a clean graph(no duplicate, sorted)
(define (graph-clean graph)
  (map (λ (node_edges)
         (let* ([head_node (car node_edges)]
                [neighbor_node (set->list (set-subtract 
                                           (list->set node_edges) (set head_node)))])
           (append (list head_node) ;neighbor_node)))
                   (sort neighbor_node
                         (λ (x y) (string<? (symbol->string x) (symbol->string y)))))))
       graph))
