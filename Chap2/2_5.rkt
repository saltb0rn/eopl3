#lang racket

(define (empty-env) null)

(define (extend-env var val env)
  (cons (cons var val) env))

(define (empty-env? env)
  (null? env))

(define (apply-env env search-var)
  (or (and (assoc search-var env) (cdr (assoc search-var env)))
      (error 'apply-env "~s is not bound" search-var)))
