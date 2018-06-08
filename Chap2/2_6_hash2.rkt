#lang racket

(define empty-env
  (let ((empty-ht (hash)))
    (lambda () empty-ht)))

(define (empty-env? env)
  (hash-empty? env))

(define (extend-env var val env)
  (hash-set env var val))

(define (apply-env env search-var)
  (or
   (and (empty-env? env)
	(error 'apply-env "~s is not bound" search-var))
   (hash-ref env search-var)))

