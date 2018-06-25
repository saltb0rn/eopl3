#lang racket

(module+ test
  (require rackunit)
  [check-equal? (number->bintree 13) '(13 () ())]
  (define t1 (insert-to-right 14 (insert-to-left 12 (number->bintree 13))))
  [check-equal? t1 '(13 (12 () ()) (14 () ()))]
  [check-equal? (move-to-left-son t1) '(12 () ())]
  [check-equal? (current-element (move-to-left-son t1)) 12]
  [check-equal? (at-leaf? (move-to-right-son (move-to-left-son t1))) #t]
  [check-equal? (insert-to-left 15 t1) '(13 (15 (12 () ()) ()) (14 () ()))]
  )

(define (number->bintree num)
  (list num null null))

(define (current-element bintree)
  (if bintree (car bintree) null))

(define (move-to-left-son bintree)
  (if bintree (second bintree) null))

(define (move-to-right-son bintree)
  (if bintree (third bintree) null))

(define (insert-to-right num bintree)
  (if (null? bintree)
      bintree
      (let ((node (current-element bintree))
	    (lson (move-to-left-son bintree))
	    (rson (move-to-right-son bintree)))
	(list node
	      lson
	      (if (null? rson)
		  (number->bintree num)
		  (list num null rson))))))

(define (insert-to-left num bintree)
  (if (null? bintree)
      bintree
      (let ((node (current-element bintree))
	    (lson (move-to-left-son bintree))
	    (rson (move-to-right-son bintree)))
	(list node
	      (if (null? lson)
		  (number->bintree num)
		  (list num lson null))
	      rson))))

(define (at-leaf? bintree)
  (null? bintree))
