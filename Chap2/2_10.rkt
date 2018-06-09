#lang racket
(require "2_5.rkt")

;(define (extend-env* varlst vallst env)
;  (if (or (null? varlst)
;	  (null? vallst)))
;  null
;  (let* ((var (car varlst))
;	 (val (car vallst))
;	 (env (extend-env var val env)))
;    (extend-env* (cdr varlst) (cdr vallst) env)))

(define (extend-env* varlst vallst env)
  (extend-env*/k varlst vallst env (lambda (val) val)))

(define (extend-env*/k varlst vallst env cont)
  (if (or (null? varlst)
	  (null? vallst))
      (cont env)
      (extend-env*/k (cdr varlst) (cdr vallst)
		     (extend-env (car varlst) (car vallst) env)
		     (lambda (val) (cont val)))))
    
