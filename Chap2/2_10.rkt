#lang racket
(require "2_5.rkt")

(define (extend-env* varlst vallst env)
  (extend-env*/k varlst vallst env (lambda (val) val)))

(define (extend-env*/k varlst vallst env cont)
  (if (or (null? varlst)
	  (null? vallst))
      (cont env)
      (extend-env*/k (cdr varlst) (cdr vallst)
		     (extend-env (car varlst) (car vallst) env)
		     (lambda (val) (cont val)))))
    
