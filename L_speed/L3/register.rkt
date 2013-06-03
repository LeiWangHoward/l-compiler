#lang plai
;check symbol is register or not
(define (l_register? symbol)
  (or (equal? symbol 'eax)
      (equal? symbol 'ebx)
      (equal? symbol 'ecx)
      (equal? symbol 'edi)
      (equal? symbol 'edx)
      (equal? symbol 'esi)))

;define a reqister list, we can think them as
; representation of color
(define register_lst
  (list 'eax 'ebx 'ecx 'edi 'edx 'esi))

;; define special lists
(define caller_save (list 'eax 'ebx 'ecx 'edx))
(define callee_save (list 'esi 'edi))
(define l_result (list 'eax))
(define x86_caller_save (list 'eax 'ecx 'edx))
(define l_args (list 'eax 'ecx 'edx))