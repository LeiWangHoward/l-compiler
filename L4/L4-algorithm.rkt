#lang plai
(require "tools.rkt")
(require "L4-type.rkt")
; find: L4-e context -> L3-e
; find takes the next step when a downward arrow points to e. k
; records the context between the arrow and the enclosing circle
(define (find e k)
  (match e
    [`(,f ,a)
     (find f (fun-ctxt a k))]
    [`(let ([,x ,r]) ,b)
     (find r (let-ctxt x b k))]
    [`(if ,c ,t ,e)
     (find c (if-ctxt t e k))]
    [(? val?);value
     (fill e k)]))
(module+ test
  (test (find '(let ([a (let ([b c]) d)])) '(let (x) (1) (2))) 'sds))
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
     (maybe-let d
                (λ (v)
                  (find a (arg-ctxt v k))))]
    [arg-ctxt
     (f k)
     (maybe-let d
                (λ (v)
                  (fill `(,f ,v) k)))]
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