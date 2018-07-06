#lang eopl

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define-datatype parse-result parse-result?
  (parsed-result
   (exp prefix-exp?)
   (rest list?)))

(define (parse-exp prefix-list)
  (cases parse-result (parse-prefix-exps prefix-list)
	 (parsed-result
	  (exp rest)
	  (or (and (null? rest) exp) (eopl:error 'parse-exp "Unused rest: ~s" rest)))))

(define (parse-prefix-exps prefix-exps)
  (if (pair? prefix-exps)
      (let ((prefix-exp (car prefix-exps))
	    (rest (cdr prefix-exps)))
	(cond
	 ((integer? prefix-exp)
	  (parsed-result
	   (const-exp prefix-exp)
	   rest))
	 ((eqv? '- prefix-exp)
	  (cases parse-result (parse-prefix-exps rest)
		 (parsed-result
		  (exp-1 rest-1)
		  (cases parse-result (parse-prefix-exps rest-1)
			 (parsed-result (exp-2 rest-2)
					(parsed-result (diff-exp exp-1 exp-2) rest-2))))))
	 (else
	  (eopl:error 'parse-exp 'parse-exp "Expected integer or -, got ~s" prefix-exp))))
      (eopl:error 'parse-exp "Not a prefix list: ~s" prefix-exps)))

(define list-1 '(- - 3 2 - 4 - 12 7))
(parse-exp list-1)
