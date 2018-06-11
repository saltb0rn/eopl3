#lang racket

(define (empty-env)
  (list
   (lambda (var)
     (error (format "~a is not bound" var)))
   (lambda () #t)
   (lambda (var) #f)))

(define (empty-env? env)
  ((second env)))

(define (has-binding? env s)
   ((third env) s))

(define (extend-env var val env)
  (list
   (lambda (search-var)
     (if (eqv? search-var var)
	 val
	 (apply-env env search-var)))
   (lambda () #f)
   (lambda (search-var)
     (or (eqv? search-var var)
	 (has-binding? env search-var)))))

(define (apply-env env search-var)
  ((first env) search-var))
