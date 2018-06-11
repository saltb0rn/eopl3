#lang racket

(provide (all-defined-out))

(define (empty-env)
  (list
   (lambda (var)
     (error (format "~a is not bound" var)))
   (lambda () #t)))

(define (empty-env? env)
  ((cadr env)))

(define (extend-env var val env)
  (list
   (lambda (search-var)
     (if (eqv? search-var var)
	 val
	 (apply-env env search-var)))
   (lambda ()
     #f)))

(define (apply-env env search-var)
  ((car env) search-var))
