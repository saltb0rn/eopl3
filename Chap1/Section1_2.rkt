#lang racket
;; 1.11
;; Because in the last line, the recursion caused by subst
;; is in sexp and sexp is a smaller substructure.


(module non-exercise racket
  ;; This is not the code for exercise, just because i want to code it.
  (provide subst)
  (define (subst new old slist)
    (subst/k new old slist (lambda (val) val)))

  (define (subst/k new old slist cont)
    (if (null? slist)
	(cont '())
	(subst-in-s-exp/k
	 new old (car slist)
	 (lambda (val1)
	   (subst/k new old (cdr slist)
		    (lambda (val2)
		      (cont (cons val1 val2))))))))

  (define (subst-in-s-exp/k new old sexp cont)
    (if (symbol? sexp)
	(if (eqv? sexp old)
	    (cont new)
	    (cont sexp))
	(subst/k new old sexp
		 (lambda (val)
		   (cont val))))))

;; 1.12
(module exercise1-12 racket
  (provide subst)
  (define (subst new old slist)
    (if (null? slist)
	'()
	(cons
	 (let ((sexp (car slist)))
	   (if (symbol? sexp)
	       (if (eqv? sexp old) new sexp)
	       (subst new old sexp)))
	 (subst new old (cdr slist))))))

;; 1.13
;; the original grammar is
;; S-list ::= ({S-exp}*))
;; S-exp  ::= Symbol | S-list
(module exercise1-13 racket
  (provide subst)
  (define (subst new old slist)
    (map
     (lambda (sexp)
       (if (symbol? sexp)
	   (if (eqv? sexp old) new sexp)
	   (subst new old sexp)))
     slist)))


(require (rename-in 'exercise1-12 (subst subst1-12)))
(require (rename-in 'exercise1-13 (subst subst1-13)))
(require (rename-in 'non-exercise (subst subst-cont)))

(let ((slist '(+ a b (+ b b) (+ c d))))
  (subst1-12 'd 'b slist)
  (subst1-13 'd 'b slist)
  (subst-cont 'd 'b slist))
