#lang eopl

(define-datatype stack stack?
  (empty-stack)
  (extend-stack
   (value number?)
   (saved-stack stack?)))

(define (pop s)
  (cases stack s
	 (empty-stack () (eopl:error 'Stack "Empty stack"))
	 (extend-stack (value saved-stack) saved-stack)))

(define (push s v)
  (extend-stack v s))

(define (top s)
  (cases stack s
	 (empty-stack () (eopl:error 'Stack "Empty stack"))
	 (extend-stack (value saved-stack) value)))

(define (empty-stack? s)
  (cases stack s
	 (empty-stack () #t)
	 (else #f)))
