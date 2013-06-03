#lang plai
(require "tools.rkt")
;;context type
(define-type context
  [let-ctxt (x var?)
            (b L4-e?)
            (k context?)]
  [if-ctxt (t L4-e?)
           (e L4-e?)
           (k context?)]
  [fun-ctxt (a (listof L4-e?))
            (k context?)]
  [arg-ctxt (f val?)
            (sub-norm (listof L4-e?))
            (sub-remain (listof L4-e?))
            (k context?)]
  [no-ctxt])

;;L4-e
(define (L4-e? e)
  (match e
    [(or (? number?);num x l
         (? var?)
         (? label?)
         ;(let ((x e)) e) (if e e e)
         `(let ((,(? var?) ,(? L4-e?))) ,(? L4-e?))
         `(if ,(? L4-e?) ,(? L4-e?) ,(? L4-e?))
         ;(e e e) 1 - 3
         `(,(? L4-e?) ...)
         ;(new-array e e)
         `(new-array ,(? L4-e?) ,(? L4-e?))
         ;(new-tuple e ...)
         `(new-tuple ,(? L4-e?) ...)
         ;(aref e e) (aset e e) (alen e)
         `(aref ,(? L4-e?) ,(? L4-e?))
         `(aset ,(? L4-e?) ,(? L4-e?) ,(? L4-e?))
         `(alen ,(? L4-e?))
         ;(begin e e)
         `(begin ,(? L4-e?) ,(? L4-e?))
         ;(print e)
         `(print ,(? L4-e?))
         ;(make-closure l e)
         `(make-closure ,(? label?) ,(? L4-e?))
         ;(closure-proc e) (closure-vars e)
         `(closure-proc ,(? L4-e?))
         `(closure-vars ,(? L4-e?))
         ;(biop e e)
         `(,(? biop?) ,(? L4-e?) ,(? L4-e?))
         `(,(? pred?) ,(? L4-e?)))
     #t]
    [_ #f]))

(module+ test
  (test (L4-e? ':label) #t)
  (test (L4-e? `(print x x)) #f)
  (test (L4-e? `(+ 1 (+ 2 2))) #t)
  (test (L4-e? `(new-tuple (+ 1 2) (+ 3 4) 4 what)) #t)
  (test (L4-e? `(let ((a (+ 3 4))) (+ a 5))) #t)
  (test (L4-e? `(let ((a (+ 3 4))) (a 8 5))) #t)
  (test (L4-e? 'print) #f)
  (test (L4-e? `(print a b c)) #f)
  (test (L4-e? `(:1 :2 :3)) #t)
  (test (L4-e? `(make-closure a 3)) #f)
  (test (L4-e? `(make-closure :a 3)) #t)
  (test (L4-e? `(number? a)) #t)
  (test (L4-e? `(what? me)) #f))