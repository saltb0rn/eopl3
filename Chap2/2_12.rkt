#lang racket

(define (empty-stack)
  (lambda ([act null])
    (cond
     ((null? act) null)
     ((or (eqv? 'pop act) (eqv? 'top act))
      (error (format "Failed to ~a from an empty stack" (symbol->string act))))
     (else (error "Empty stack")))))

(define (empty-stack? stack)
  (null? (stack)))

(define (push stack val)
  (lambda ([act null])
    (cond
     ((null? act) (list val stack))
     ((eqv? 'top act) val)
     ((eqv? 'pop act) stack)
     (else (error "Unknown action to stack")))))

(define (top stack)
  (stack 'top))

(define (pop stack)
  (stack 'pop))
