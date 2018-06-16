#lang racket

(module+ test
  (require rackunit)
  [check-true (var-exp? 'a)]
  [check-false (var-exp? 1)]
  [check-false (var-exp? '(lambda (val) val))]
  [check-false (var-exp? '((lambda (val) val) (lambda (val2) val2)))]
  [check-true (lambda-exp? '(lambda (val) val))]
  [check-true (lambda-exp? '(lambda (val) (lambda (val2) val2)))]
  [check-false (lambda-exp? 'a)]
  [check-false (lambda-exp? '((lambda (val) val) (lambda (val2) val2)))]
  [check-false (lambda-exp? '(a b))]
  [check-true (app-exp? '((lambda (val) (val a)) (lambda (val1) val1)))]
  [check-true (app-exp? '(a b))]
  [check-equal? (var-exp->var 'a) 'a]
  [check-true (var-exp? (var-exp 'a))]
  [check-exn exn:misc:match? (lambda () (var-exp->var '(lambda (val) val)))]
  [check-equal? (lambda-exp 'var 'var) '(lambda (var) var)]
  [check-true (lambda-exp? (lambda-exp 'var 'var))]
  [check-equal? (app-exp '(lambda (var) var) '(lambda (var2) var2))
		'((lambda (var) var) (lambda (var2) var2))]
  )

(define (var-exp var) var)

(define (lambda-exp var lcexp) (list 'lambda (list var) lcexp))

(define (app-exp rantor rand) (list rantor rand))

(define (var-exp? lcexp) (symbol? lcexp))

(define (lambda-exp? lcexp)
  (match lcexp
    ((list operator (list identifier) lc-exp)
     (and (eqv? operator 'lambda)
	  (var-exp? identifier)
	  (or (var-exp? lc-exp)
	      (lambda-exp? lc-exp)
	      (app-exp? lc-exp))))
    (_ #f)))

(define (app-exp? lc-exp)
  (match lc-exp
    ((list rantor rand)
     (and
      (or
       (var-exp? rantor)
       (lambda-exp? rantor)
       (app-exp? rantor))
      (or
       (var-exp? rand)
       (lambda-exp? rand)
       (app-exp? rand))))
    (_ #f)))

(define (var-exp->var lc-exp)
  (match lc-exp
    ((? var-exp? identifier) identifier)))

(define (lambda-exp->bound-var lc-exp)
  (match lc-exp
    ((list operator (list identifier) exp) identifier)))

(define (lambda-exp->body lc-exp)
  (match lc-exp
    ((list operator (list identifier) exp) exp)))

(define (app-exp->rator lc-exp)
  (match lc-exp
    ((list rantor rand) rantor)))

(define (app-exp->rand lc-exp)
  (match lc-exp
    ((list rantor rand) rand)))
