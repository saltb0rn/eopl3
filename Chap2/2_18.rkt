#lang racket

(module+ test
  (require rackunit)

  [check-equal? (number->sequence 7) '(7 () ())]
  [check-equal? (current-element '(6 (5 4 3 2 1) (7 8 9))) 6]
  [check-equal? (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9))]
  [check-equal? (move-to-left '(6 () (7 8 9))) '(6 () (7 8 9))]
  [check-equal? (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9))]
  [check-equal? (move-to-right '(6 (5 4 3 2 1) ())) '(6 (5 4 3 2 1) ())]
  [check-equal? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9))]
  [check-equal? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9))]
  )

(define (number->sequence n)
  (list n null null))

(define (current-element node-in-seq)
  (car node-in-seq))

(define (left-list node-in-seq)
  (second node-in-seq))

(define (right-list node-in-seq)
  (third node-in-seq))

(define (move-to-left node-in-seq)
  (move-to 'left node-in-seq))

(define (move-to-right node-in-seq)
  (move-to 'right node-in-seq))

(define (move-to direction node-in-seq)
  (let ((node (current-element node-in-seq))
	(llst (left-list node-in-seq))
	(rlst (right-list node-in-seq)))
    (cond
     [(eq? direction 'left)
      (if (null? llst)
	  node-in-seq
	  (list (car llst)
		(cdr llst)
		(cons node rlst)))]
     [(eq? direction 'right)
      (if (null? rlst)
	  node-in-seq
	  (list (car rlst)
		(cons node llst)
		(cdr rlst)))]
     [else
      (error 'direction "No direction named ~a" direction)])))

(define (insert-to-left elm node-in-seq)
  (insert-to 'left elm node-in-seq))

(define (insert-to-right elm node-in-seq)
  (insert-to 'right elm node-in-seq))

(define (insert-to direction elm node-in-seq)
  (let ((node (current-element node-in-seq))
	(llst (left-list node-in-seq))
	(rlst (right-list node-in-seq)))
    (cond
     [(eq? direction 'left)
      (list node (cons elm llst) rlst)]
     [(eq? direction 'right)
      (list node llst (cons elm rlst))]
     [else
      (error 'direction "No directio named ~a" direction)])))
