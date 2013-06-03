#lang plai
;;test if tools function well
(require "tools.rkt")
(module+ test
  (test (spilled? 's_a 's_) #t)
  (test (spilled? 's_2 '2_) #f)
  (test (sort-by-length '((a eax ebx ecx edi) (x0 eax ebx ecx edi edx)
                                              (x1 eax ebx ecx edi edx esi) (x2 ebx) (x3 ebx edi esi)))
        '( (x2 ebx) (x3 ebx edi esi) (a eax ebx ecx edi) (x0 eax ebx ecx edi edx)
                    (x1 eax ebx ecx edi edx esi))))

(require "graph.rkt")
;;test functions(operations) for graph
(module+ test
  (test (get-var-lst '((r w) (eax f) (ebx edx)) '((esi edi) (c ecx))) '((r) (w) (f) (c))))
  ;(test (init-graph-color register_graph '((a) (b) (c) (d))) 's 'd))
(require "L2.rkt")
;;substitute in register assign
(module+ test
  (test 
   (l2-to-l1 '((cow grass) (me eax)) '((cow is me) (cow is you)))
   (list (list 'grass 'is 'eax) (list 'grass 'is 'you))))

