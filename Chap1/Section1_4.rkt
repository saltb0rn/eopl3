#lang racket

(provide duple
	 invert
	 down
	 swapper
	 list-set
	 count-occurences
	 product
	 filter-in
	 list-index
	 every?
	 exists?
	 up
	 flatten)


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
      (if (zero? n)
	  (list-set/k (cdr lst) (sub1 n) x
		      (lambda (val) (cont (cons x val))))
	  (list-set/k (cdr lst) (sub1 n) x
		      (lambda (val) (cont (cons (car lst) val))))
	  ;;(list-set/k (cdr lst) (sub1 n) x
	  ;;	    (cont
	  ;;	     (cons
	  ;;	      (if (zero? n) x (car lst))
	  ;;	      val))))))
	  )))

;; 1.20
;; count-occurrences : symbol? * (listof symbol?) -> number?
(define (count-occurences s slist)
  (count-occurences/k s slist (lambda (val) val)))

;; count-occurences/k : symbol? * (listof symbol?) * cont -> number?
(define (count-occurences/k s slist cont)
  (if (null? slist)
      (cont 0)
      (let ((sexp (car slist)))
	(if (symbol? sexp)
	    (if (eqv? s sexp)
		(count-occurences/k s (cdr slist)
				    (lambda (val)
				      (cont (add1 val))))
		(count-occurences/k s (cdr slist)
				    (lambda (val)
				      (cont val))))
	    (count-occurences/k
	     s sexp
	     (lambda (val1)
	       (count-occurences/k
		s (cdr slist)
		(lambda (val2)
		  (cont (+ val1 val2))))))
	    ;;; also, we can rewrite the false branch into
	    ;;(count-occurences/k s (cdr slist)
	    ;;		  (lambda (val1)
	    ;;			    (let ((sexp (car slist)))
	    ;;			      (if (symbol? sexp)
	    ;;				  (if (eqv? s sexp)
	    ;;				      (cont (add1 val1))
	    ;;				      (cont val1))
	    ;;				  (count-occurences/k
	    ;;				   s sexp
	    ;;				   (lambda (val2)
	    ;;				     (cont (+ val1 val2))))))))))
	    ))))

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
	    (cont (append val2 val1))))))))

;; combine-one : symbol? * list? * cont -> (listof pair?)
(define (combine-one/k s sos2 cont)
  (if (null? sos2)
      (cont '())
      (combine-one/k
       s (cdr sos2)
       (lambda (val1)
	 (cont
	  (cons (cons s (list (car sos2))) val1))))))

;; 1.22
;; filter-in : predicate? * list? -> list?
(define (filter-in pred lst)
  (filter-in/k pred lst (lambda (val) val)))

(define (filter-in/k pred lst cont)
  (if (null? lst)
      (cont '())
      (let ([elm (car lst)])
	(if (pred elm)
	    (filter-in/k
	     pred (cdr lst)
	     (lambda (val)
	       (cont (cons elm val))))
	    (filter-in/k
	     pred (cdr lst)
	     (lambda (val)
	       (cont val)))))
      ;;; also, we can rewrite the false branch into
      ;;      (filter-in/k
      ;;       pred (cdr lst)
      ;;       (lambda (val)
      ;;	 (let ([elm (car lst)])
      ;;	   (if (pred elm)
      ;;	       (cont (cons elm val))
      ;;	       (cont val)))
      ))

;; 1.23
;; list-index : predicate * list? -> (or/c number? #f)
(define (list-index pred lst)
  (list-index/k pred lst (lambda (val) val)))

;; list-index/k : predicate * list? * cont -> (or/c number? #f)
(define (list-index/k pred lst cont)
  (cond
   [(null? lst) => (lambda (val) (cont (not val)))]
   [(pred (car lst))
    (list-index/k pred (cdr lst)
		  (lambda (val) (cont 0)))]
   [else
    (count-index/k pred (cdr lst)
		   (lambda (val)
		     (if (boolean? val)
			 val
			 (cont (add1 val)))))]
   ;;; as an alternative, you can use the below line
   ;;[else (count-index/k pred lst cont)]
   ))

(define (count-index/k pred lst cont)
  (if (null? lst)
      (cont 0)
      (let ((elm (car lst)))
	(if (pred elm)
	    (count-index/k pred (cdr lst)
			   (lambda (val) (cont 0)))
	    (if (null? (cdr lst))
		#f
		(count-index/k pred (cdr lst)
			       (lambda (val) (cont (add1 val)))))))))

;; 1.24
;; every? : predicate * list? -> boolean?
(define (every? pred lst)
  (every?/k pred lst (lambda (val) val)))

;; every?/k : predicate * list? * cont -> boolean?
(define (every?/k pred lst cont)
  (if (null? lst)
      (cont #t)
      (every?/k pred (cdr lst)
		(lambda (val)
		  (if (pred (car lst))
		      (cont val)
		      #f)))))


;; 1.25
;; exists? : predicate * list? -> boolean?
(define (exists? pred lst)
  (exists?/k pred lst (lambda (val) val)))

;; exists?/k : predicate * list? * cont -> boolean?
(define (exists?/k pred lst cont)
  (if (null? lst)
      (cont #f)
      (exists?/k pred (cdr lst)
		 (lambda (val)
		   (if (pred (car lst))
		       #t
		       (cont val))))))

;; 1.26
;; up : list? -> list?
(define (up lst)
  (up/k lst (lambda (val) val))
  ;;(if (null? lst)
  ;;    '()
  ;;    (if (list? (car lst))
  ;;	  (append (car lst) (up (cdr lst)))
  ;;	  (cons (car lst) (up (cdr lst))))))
  )

;; up/k : list? * cont -> list?
(define (up/k lst cont)
  (if (null? lst)
      (cont '())
      (if (list? (car lst))
	  (up/k (cdr lst)
		(lambda (val)
		  (cont (append (car lst) val))))
	  (up/k (cdr lst)
		(lambda (val)
		  (cont (cons (car lst) val)))))))

;; 1.27
;; flatten : (listof symbol?) -> (listof symbol?)
(define (flatten slist)
  (flatten/k slist (lambda (val) val)))

;; flatten/k : (listof symbol?) * cont -> (listof symbol?)
(define (flatten/k slist cont)
  (if (null? slist)
      (cont '())
      (let ((elm (car slist)))
	(if (list? elm)
	    (flatten/k
	     (cdr slist)
	     (lambda (val1)
	       (flatten/k
		elm
		(lambda (val2)
		  (cont
		   (append val2 val1))))))
	    (flatten/k
	     (cdr slist)
	     (lambda (val)
	       (cont (cons elm val))))))))
