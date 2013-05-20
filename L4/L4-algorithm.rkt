#lang plai
(require "tools.rkt")
(require "L4-type.rkt")
; find: L4-e context -> L3-e
; find takes the next step when a downward arrow points to e. k
; records the context between the arrow and the enclosing circle
(define (find e k)
  (match e
    ;;eliminate begin
    [`(begin ,e1 ,e2)
     (find `(let ([,(fresh-var) ,e1]) ,e2) k)]
    [`(let ([,x ,r]) ,b)
     (find r (let-ctxt x b k))]
    [`(if ,c ,t ,e)
     (find c (if-ctxt t e k))]
    ;[`(,(? biop? p) ,e1 ,e2)
    ; (find p (fun-ctxt `(,e1 ,e2) k))]
    ;[`(,(? L4-key? fun-name) ,a ...)
    ; (find fun-name (fun-ctxt a k))]
    [`(,f ,a ...)
     (find f (fun-ctxt a k))]
    [(? val?);value
     (fill e k)]))
(module+ test
  (test (norm '(let ([a (let ([b (let ([c d]) e)]) f)]) g)) 
        '(let ((c d)) (let ((b e)) (let ((a f)) g))))
  (test (norm '(let ([a (let ([b (let ([c (aref d e)]) f)]) g)]) h)) 
        '(let ((_var_0 d)) (let ((c e)) (let ((b f)) (let ((a g)) h)))))
  (test (norm '(if a (begin a b) (print (+ c d)))) 'sdsd))
;;'(let ((c (d e))) (let ((b f)) (let ((a g)) h))))))

; fill: L3-d context -> L3-e
; fill does the same for an upward arrow
(define (fill d k)
  (type-case context k
    [let-ctxt
     (x b k)
     `(let ([,x ,d])
        ,(find b k))]
    [if-ctxt
     (t e k)
     (maybe-let d
                (λ (v)
                  `(if ,v
                       ,(find t k)
                       ,(find e k))))]
    [fun-ctxt
     (a k)
     ;(if (empty? a)
     ;    (maybe-let d
     ;               (λ (v)
     ;                 (fill `(,v) k)))
     (maybe-let d
                (λ (v)
                  (find (first a)
                        (arg-ctxt v
                                  '()
                                  (rest a)
                                  k))))]
    [arg-ctxt
     (f sub-norm sub-remain k)
     (if (empty? sub-remain)
         (maybe-let d
                    (λ (v)
                      (fill `(,f ,sub-norm ,v) k)))
         (maybe-let d
                    (λ (v)
                      (find (first sub-remain)
                            (arg-ctxt f
                                      `(,sub-norm ,v)
                                      (rest sub-remain)
                                      k)))))]
    [no-ctxt () d]))

; maybe-let: L3-d (val -> L3-e) -> L3-e
(define (maybe-let d f)
  (if (val? d)
      (f d)
      (let ([x (fresh-var)])
        `(let ([,x ,d])
           ,(f x)))))
;; norm: L4-e -> L3-e
(define (norm e)
  (find e (no-ctxt)))