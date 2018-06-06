#lang racket

(module+ test
  (require rackunit)

  ;; for 2.1
  [check-equal? (plus (make-integer 2) (make-integer 3)) (make-integer 5)]
  [check-equal? (multi (make-integer 2) (make-integer 3)) (make-integer 6)]
  [check-equal? (factorial (make-integer 3)) (make-integer 6)]

  )

;; The client
(define (plus x y)
  (if (is-zero? x)
      y
      (successor (plus (predecessor x) y))))

(define (multi x y)
  (if (is-zero? x)
      (zero)
      [if (eqv? x (make-integer 1))
	  y
	  (plus y (multi (predecessor x) y))]))

(define (factorial n)
  (if (or (is-zero? n)
	  (eqv? n (make-integer 1)))
      (make-integer 1)
      (multi n (factorial (predecessor n)))))

(define (make-integer n)
  (if (= n 0)
      '()
      (if (< n N)
	  (list n)
	  (cons (remainder n N)
		(make-integer (truncate (/ n N)))))))

;; 2.1

(define N 16)
(define (zero) '())
(define (is-zero? n) (null? n))
(define (successor n)
  (if (is-zero? n)
      (make-integer 1)
      [if (< (car n) (- N 1))
	  [cons (+ (car n) 1) (cdr n)]
	  [cons 0
		(if (is-zero? (cdr n))
		    (successor '())
		    (successor (cons (cadr n) (cddr n))))]]))

(define (predecessor n)
  (let ([r (car n)]
	[q (cdr n)])
    (if (null? q)
	[if (zero? (- r 1)) '() `(,(- r 1))] ; single number
	[if (zero? r)                        ; multi number
	    [cons (- N 1)
		  (predecessor q)]
	    [cons (- r 1) q]])))
