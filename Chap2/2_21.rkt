#lang eopl

(define-datatype environment environment?
  (empty-env)
  (extend-env (identifier symbol?)
	      (value number?)
	      (saved-env environment?)))

(define (apply-env env searched-var)
  (cases environment env
	 (empty-env () (eopl:error 'Environment "~a is not bound" searched-var))
	 (extend-env (identifier value saved-env)
		     (if (eqv? identifier searched-var)
			 value
			 (apply-env saved-env searched-var)))))
