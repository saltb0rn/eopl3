#lang racket

(provide duple
	 invert
	 down)


;; 1.15
;; duple : number? * any? -> (listof any?)
(define (duple n x)
  (duple/k n x (lambda (val) val)))

;; duple : number? * any? * cont -> (list any?)
(define (duple/k n x cont)
  (if (<= n 0)
      (cont '())
      (duple/k (sub1 n) x
	       (lambda (val)
		 (cont (cons x val))))))


;; 1.16
;; invert : (listof pair?) -> (listof pair?)
(define (invert lst)
  (invert/k lst (lambda (val) val)))

;; invert : (listof pair?) * cont -> (listof pair?)
(define (invert/k lst cont)
  (if (null? lst)
      (cont '())
      (let ((first-one (first lst))
	    (the-rest (cdr lst)))
	(invert/k the-rest
		  (lambda (val)
		    (cont
		     (cons
		      (cons (last first-one)
			    (list (car first-one)))
		      val)))))))

;; 1.17
;; down : list? -> list?
(define (down lst)
  (down/k lst (lambda (val) val)))

;; down/k : list? * cont -> list?
(define (down/k lst cont)
  (if (null? lst)
      (cont '())
      (down/k (cdr lst)
	      (lambda (val)
		(append (cont val)
			(list (list (car lst))))))))


;; 1.18
;; swapper : symbol? * symbol? * (listof symbol?) -> (listof symbol?)
(define (swapper s1 s2 slist)
  (swapper/k s1 s2 slist (lambda (val) val)))

;; swapper/k : symbol? * symbol? * (listof symbol?) * cont -> (listof symbol?)
(define (swapper/k s1 s2 slist cont)
  (if (null? slist)
      (cont '())
      (let ((sexp (car slist))
	    (the-rest (cdr slist)))
	(if (symbol? sexp)
	    (cond
	     ((eqv? sexp s1) (swapper/k
			      s1 s2 the-rest
			      (lambda (val)
				(cont (cons s2 val)))))
	     ((eqv? sexp s2) (swapper/k
			      s1 s2 the-rest
			      (lambda (val)
				(cont (cons s1 val)))))
	     (else (swapper/k
		    s1 s2 the-rest
		    (lambda (val)
		      (cont (cons sexp val))))))
	    (swapper/k
	     s1 s2 sexp
	     (lambda (val1)
	       (swapper/k
		s1 s2 the-rest
		(lambda (val2)
		  (cont (cons val1 val2))))))))))
