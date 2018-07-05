#lang eopl
;; not tested yet.

(define-datatype red-blue-tree red-blue-tree?
  (rb-tree (rb-subtree red-blue-subtree?)))

(define-datatype red-blue-subtree red-blue-subtree?
  (red-node (ltree red-blue-subtree?)
	    (rtree red-blue-subtree?))
  (blue-node (subtrees (list-of red-blue-subtree?)))
  (leaf-node (value integer?)))

(define (mark-leaves-with-red-depth rbtree)
  (cases red-blue-tree rbtree
	 (rb-tree (rb-subtree)
		  (rb-tree (sub-mark-leaves-with-red-depth rb-subtree 0)))))

(define (sub-mark-leaves-with-red-depth rbstree red-depth)
  (cases red-blue-subtree rbstree
	 (red-node (ltree rtree)
		   (red-node
		    (sub-mark-leaves-with-red-depth ltree (+ red-depth 1))
		    (sub-mark-leaves-with-red-depth rtree (+ red-depth 1))))
	 (blue-node (subtrees)
		    (blue-node
		     (map (lambda (st) (sub-mark-leaves-with-red-depth st red-depth))
			  subtrees)))
	 (leaf-node (num)
		    (leaf-node red-depth))))
