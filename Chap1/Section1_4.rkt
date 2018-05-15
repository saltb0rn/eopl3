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

;; 1.19
;; list-set : list? * number? * any -> list?
(define (list-set lst n x)
  (list-set/k lst n x (lambda (val) val)))

;; list-set/k list? * number? * any * cont -> list?
(define (list-set/k lst n x cont)
  (if (null? lst)
      (cont '())
      (list-set/k (cdr lst) (sub1 n) x
		  (lambda (val)
		    (cont
		     (cons
		      (if (zero? n) x (car lst))
		      val))))))

;; 1.20
;; count-occurrences : symbol? * (listof symbol?) -> number?
(define (count-occurences s slist)
  (count-occurences/k s slist (lambda (val) val)))

;; count-occurences/k : symbol? * (listof symbol?) * cont -> number?
(define (count-occurences/k s slist cont)
  (if (null? slist)
      (cont 0)
      (count-occurences/k s (cdr slist)
			  (lambda (val1)
			    (let ((sexp (car slist)))
			      (if (symbol? sexp)
				  (if (eqv? s sexp)
				      (cont (add1 val1))
				      (cont val1))
				  (count-occurences/k
				   s sexp
				   (lambda (val2)
				     (cont (+ val1 val2))))))))))

;; 1.21
;; product : list? * list? -> (listof pair?)
(define (product sos1 sos2)
  (product/k sos1 sos2 (lambda (val) val)))

;; product : list? * list? * cont -> (listof pair?)
(define (product/k sos1 sos2 cont)
  (if (or (null? sos1) (null? sos2))
      (cont '())
      (product/k
       (cdr sos1) sos2
       (lambda (val1)
	 (combine-one/k
	  (car sos1) sos2
	  (lambda (val2)
	    (cont (cons val2 val1))))))))

;; combine-one : symbol? * list? * cont -> (listof pair?)
(define (combine-one/k s sos2 cont)
  (if (null? sos2)
      (cont '())
      (combine-one/k
       s (cdr sos2)
       (lambda (val1)
	 (cont
	  (cons (cons s (list (car sos2))) val1))))))
