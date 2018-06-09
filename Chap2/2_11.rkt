#lang racket

(define (empty-env) null)

(define (empty-env? env) (null? env))

(define (extend-env var val env)
  (cons (list (list var) (list val)) env))

(define (extend-env* varlst vallst env)
  (cons (list (reverse varlst) (reverse vallst)) env))

(define (env->vars env)
  (let ((left-ribs (map (lambda (ribs) (car ribs)) env)))
    (append* left-ribs)))

(define (env->vals env)
  (let ((right-ribs (map (lambda (ribs) (cadr ribs)) env)))
    (append* right-ribs)))

(define (apply-env env search-var)
  (let* ((vars (env->vars env))
	 (vals (env->vals env))
	 (index (index-of vars search-var)))
    (if index
	(list-ref vals index)
	(error 'apply-env "~s is not bound" search-var))))
