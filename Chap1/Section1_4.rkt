#lang racket

(module+ test
  (require rackunit)
  ;; for 1.15
  [check-equal? (duple 2 3) '(3 3)]
  [check-equal? (duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha))]
  [check-equal? (duple 0 '(blah)) '()]

  ;; for 1.16
  [check-equal? (invert '((a 1) (a 2) (1 b) (2 b))) '((1 a) (2 a) (b 1) (b 2))]
  [check-equal? (invert '()) '()]

  ;; for 1.17
  [check-equal? (down '(1 2 3)) '((1) (2) (3))]
  [check-equal? (down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea)))]
  [check-equal? (down '(a (more (complicated)) object)) '((a) ((more (complicated))) (object))]

  ;; for 1.18
  [check-equal? (swapper 'a 'd '(a b c d)) '(d b c a)]
  [check-equal? (swapper 'a 'd '(a d () c d)) '(d a () c a)]
  [check-equal? (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y)))]

  ;; for 1.19
  [check-equal? (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d)]
  [check-equal? (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3) '(1 5 10)]

  ;; for 1.20
  [check-equal? (count-occurences 'x '((f x) y (((x z) x)))) 3]
  [check-equal? (count-occurences 'x '((f x) y (((x z) () x)))) 3]
  [check-equal? (count-occurences 'w '((f x) y ((x z) x))) 0]

  ;; for 1.21
  [check-equal? (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y))]

  ;; for 1.22
  [check-equal? (filter-in number? '(a 2 (1 3) b 7)) '(2 7)]
  [check-equal? (filter-in symbol? '(a (b c) 17 foo)) '(a foo)]

  ;; for 1.23
  [check-equal? (list-index number? '(a 2 (1 3) b 7)) 1]
  [check-equal? (list-index symbol? '(a (b c) 17 foo)) 0]
  [check-equal? (list-index symbol? '(1 2 (a b) 3)) #f]

  ;; for 1.24
  [check-equal? (every? number? '(a b c 3 e)) #f]
  [check-equal? (every? number? '(1 2 3 5 4)) #t]

  ;; for 1.25
  [check-equal? (exists? number? '(a b c 3 e)) #t]
  [check-equal? (exists? number? '(a b c d e)) #f]

  ;; for 1.26
  [check-equal? (up '((1 2) (3 4))) '(1 2 3 4)]
  [check-equal? (up '((x (y)) z)) '(x (y) z)]

  ;; for 1.27
  [check-equal? (flatten '(a b c)) '(a b c)]
  [check-equal? (flatten '((a) () (b ()) () (c))) '(a b c)]
  [check-equal? (flatten '((a b) c (((d)) e))) '(a b c d e)]
  [check-equal? (flatten '(a b (() (c)))) '(a b c)]

  ;; for 1.28
  [check-equal? (merge '(1 4) '(1 2 8)) '(1 1 2 4 8)]
  [check-equal? (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91)]

;; for 1.29
  [check-equal? (sort '(8 2 5 2 3)) '(2 2 3 5 8)]

  ;; for 1.30
  [check-equal? (sort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8)]
  [check-equal? (sort/predicate > '(8 2 5 2 3)) '(8 5 3 2 2)]

  ;; for 1.33
  [check-equal? (mark-leaves-with-red-depth
		 (interior-node 'red
				(interior-node 'bar
					       (leaf 26)
					       (leaf 12))
				(interior-node 'red
					       (leaf 11)
					       (interior-node 'quux
							      (leaf 117)
							      (leaf 14)))))
		'(red
		  (bar 1 1)
		  (red 2 (quux 2 2)))]

  ;; 1.34
  [check-equal? (path 17 '(14 (7 () (12 () ()))
			      (26 (20 (17 () ())
				      ())
				  (31 () ()))))
		'(right left left)]

  [check-equal? (number-leaves
		 (interior-node 'foo
				(interior-node 'bar
					       (leaf 26)
					       (leaf 12))
				(interior-node 'baz
					       (leaf 11)
					       (interior-node 'quux
							      (leaf 117)
							      (leaf 14)))))
		'(foo
		  (bar 0 1)
		  (baz
		   2
		   (quux 3 4)))]

  ;; 1.36
  [check-equal? (number-elements '(R a c k e t))
		'((0 R) (1 a) (2 c) (3 k) (4 e) (5 t))]

  [check-equal? (number-elements '()) '()]

  )




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

;; 1.28
;; merge : (listof integer?) * (listof integer?) -> (listof integer?)
(define (merge loi1 loi2)
  (sort (append loi1 loi2)))

;; 1.29
;; sort : (listof integer?) -> (listof integer)
(define (sort loi)
  (sort-tail loi '()))

(define (sort-tail loi res)
  (if (null? loi)
      res
      (sort-tail (cdr loi)
		 (insert-to-list/pred/k < (car loi) res (lambda (val) val)))))

;; it is weird to rewrite the tail-call fucntion to the CPS function
;;(define (sort/k loi res cont)
;;  (if (null? loi)
;;      (cont res)
;;      (sort/k (cdr loi)
;;	      (insert-to-list/pred/k < (car loi) res cont)
;;	      (lambda (val)
;;		(cont val)))))

;; 1.30
;; sort/predicate : pred * loi -> loi
(define (sort/predicate pred loi)
  (sort/predicate-tail pred loi '()))

(define (sort/predicate-tail pred loi res)
  (if (null? loi)
      res
      (sort/predicate-tail pred (cdr loi)
			   (insert-to-list/pred/k pred (car loi) res (lambda (val) val)))))

(define (insert-to-list/pred/k pred num loi cont)
  (if (null? loi)
      (cont (cons num loi))
      (let ((head (car loi)))
	(cond
	 ((pred num head) (cont (cons num loi)))
	 (else (insert-to-list/pred/k
		pred num (cdr loi)
		(lambda (val)
		  (cont (cons head val)))))))))

;; 1.31
;; the definition 1.1.7 is on page 32

(define (leaf content)
  content)

(define (interior-node sym lson rson)
  (list sym lson rson))

(define (leaf? tree)
  [match tree
    [(list sym lson rson) #f]
    [_ #t]])

(define (lson tree)
  [match tree
    [(list sym lson rson) lson]
    [_ '()]])

(define (rson tree)
  [match tree
    [(list sym lson rson) rson]
    [_ '()]])

(define (contents-of tree)
  (or (and (leaf? tree) tree)
      (car tree)))

;; 1.32
;; double-tree : (listof tree) -> (lisetof tree)
(define (double-tree tree)
  (double-tree/k tree (lambda (val) val)))

(define (double-tree/k tree cont)
  (cond
   [(null? tree) (cont '())]
   [(leaf? tree) (cont (* 2 (contents-of tree)))]
   [else
    (double-tree/k
     (lson tree) (lambda (lval)
		   (double-tree/k
		    (rson tree) (lambda (rval)
				  (cont (interior-node (car tree) lval rval))))))]))

;; 1.33
(define (mark-leaves-with-red-depth tree)
  (mark-leaves-with-red-depth/k tree 0 (lambda (val) val)))

(define (mark-leaves-with-red-depth/k tree depth cont)
  (cond
   [(or (null? tree) (leaf? tree))
    (cont depth)]
   [else
    (mark-leaves-with-red-depth/k
     (lson tree) (or (and (eq? (car tree) 'red) (+ depth 1)) depth)
     (lambda (lval)
       (mark-leaves-with-red-depth/k
	(rson tree) (or (and (eq? (car tree) 'red) (+ depth 1)) depth)
	(lambda (rval)
	  (cont (interior-node (car tree) lval rval))))))]))

;; 1.34
(define (path n bst)
  (path/k n bst (lambda (val) val)))

(define (path/k n bst cont)
  (let ((node (contents-of bst)))
    (cond
     [(or (null? bst) (equal? n node))
      (cont '())]
     [(< n node)
      (path/k n (lson bst)
	      (lambda (val)
		(cont (cons 'left val))))]
     [(> n node)
      (path/k n (rson bst)
	      (lambda (val)
		(cont (cons 'right val))))])))

;; 1.35
(define (number-leaves bt)
  (car
   (number-leaves-from/k bt 0 (lambda (val) val))))

(define (number-leaves-from/k bt n cont)
  (cond
   [(null? bt) (cont (list bt n))]
   [(leaf? bt) (cont (list n (+ n 1)))]
   [else
    (number-leaves-from/k
     (lson bt) n
     (lambda (lval)
       (number-leaves-from/k
	(rson bt) (cadr lval)
	(lambda (rval)
	  (cont
	   (list (interior-node
		  (contents-of bt)
		  (car lval)
		  (car rval))
		  (cadr rval)))))))]))

;; 1.36
(define (g node lst)
  (g/k node lst 0 (lambda (val) val)))

(define (g/k node lst counter cont)
  (if (null? lst)
      (cont (list node))
      (g/k
       (list (+ counter 1) (cadr (car lst)))
       (cdr lst)
       (+ counter 1)
       (lambda (val)
	 (cont (cons node val))))))

(define (number-elements lst)
  (if (null? lst)
      '()
      (g (list 0 (car lst)) (number-elements (cdr lst)))))
	 




      


  
