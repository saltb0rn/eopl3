#lang racket

(struct empty-environment ())

(struct extend-environemt (var val env))

(define (empty-env)
  (empty-environment))

(define (empty-env? env)
  (empty-environment? env))

(define (extend-env var val env)
  (extend-environemt var val env))

(define (apply-env env search-var)
  (or (and (empty-env? env)
	   (error 'apply-env "~s is not bound" search-var))
      (and (equal? (extend-environemt-var env) search-var)
	   (extend-environemt-val env))
      (apply-env (extend-environemt-env env) search-var)))
