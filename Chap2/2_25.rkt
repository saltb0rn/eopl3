#lang eopl

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define-datatype result result?
  (empty-result)
  (a-result (key symbol?)
	    (sum (lambda (val) (or null? number?)))))

(define (interior->sum interior)
  (interior->sum/k interior (lambda (val) val)))

(define (interior->sum/k interior cont)
  (cases bintree interior
	 (leaf-node (num) (cont num))
	 (interior-node (key left right)
			(interior->sum/k
			 left
			 (lambda (val1)
			   (interior->sum/k
			    right
			    (lambda (val2)
			      (cont (+ val1 val2)))))))))

(define (empty-result? res)
  (cases result res
	 (empty-result () #t)
	 (else #f)))

(define (remove-empty-result lst)
  (sub-remove-empty-result lst '()))

(define (sub-remove-empty-result lst res)
  (if (null? lst)
      res
      (sub-remove-empty-result
       (cdr lst)
       (cases result (car lst)
	      (empty-result () res)
	      (else (append res (list (car lst))))))))

(define (the-max lst)
  (if (null? lst)
      (eopl:error 'the-max "list is not allowed to be empty")
      (the-max-sub (cdr lst) (car lst))))

(define (the-max-sub lst max)
  (if (null? lst)
      max
      (the-max-sub
       (cdr lst)
       (cases result (car lst)
	      (empty-result () (car lst))
	      (else max)))))

(define (max-interior bt)
  (cases result (sub-max-interior bt)
	 (a-result (key sum) key)
	 (else (eopl:error 'result "result should not be empty"))))

(define (sub-max-interior bt)
  (cases bintree bt
	 (leaf-node (num) (empty-result))
	 (interior-node (key left right)
			(let ((nsum (a-result key (interior->sum bt)))
			      (lsum (sub-max-interior left))
			      (rsum (sub-max-interior right)))
			  (the-max (remove-empty-result (list nsum lsum rsum)))))))


(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))
(max-interior tree-2)
(max-interior tree-3)
