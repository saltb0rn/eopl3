#lang racket
;; https://github.com/svenpanne/EOPL3/blob/master/chapter2/exercise-2-20.rkt
;; I copy the code and write down the representations.

;; BinTree ::= Leaf | (fork node BinTree BinTree)
;; Leaf :: ()
;; Loc ::= (BinTree . Context)
;; Context ::= TOP | (CtxElm . Context)
;; TOP ::= ()
;; CtxElm ::= ('left node BinTree) | ('right node BinTree)

(define leaf null)
(define at-leaf? null?)

(define (fork n l r) (list n l r))
(define current-element car)
(define move-to-left cadr)
(define move-to-right caddr)

(define (number->bintree n) (fork n leaf leaf))

(define (insert-to-left n t)
  (fork (current-element t)
	(fork n (move-to-left t) leaf)
	(move-to-right t)))

(define (insert-to-right n t)
  (fork (current-element t)
	(move-to-left t)
	(fork n leaf (move-to-right t))))

;; A location in te binary as a pair of a tree (the current subtree)
;; and a context (the steps to reconstruct the initial tree).
(define location cons)
(define location->tree car)
(define location->context cdr)

;; A context is a stack of context elements.
(define (root-context) null)
(define root-context? null?)
(define push-context-element cons)
(define context->top car)
(define context->parent cdr)

;; A context element consists of a direction, a value and a tree.
(define (context-element d n t) (list d n t))
(define context-element->tag car)
(define context-element->value cadr)
(define context-element->tree caddr)
(define (left n t) (context-element 'left n t))
(define (right n t) (context-element 'right n t))
(define (left? e) (eqv? (context-element->tag e) 'left))

(define (number->bintree-loc n)
  (location (number->bintree n) (root-context)))

(define (current-element-loc l)
  (current-element (location->tree 1)))

(define (move-up-loc l)
  (let ((context (location->context l)))
    (if (root-context? context)
	(error 'move-up "already at root: ~s" l)
	(let ((top (context->top context)))
	  (location (if (left? top)
			(fork (context-element->value top)
			      (location->tree l)
			      (context-element->tree top))
			(fork (context-element->value top)
			      (context-element->tree top)
			      (location->tree l)))
		    (context->parent context))))))

(define (move-to-left-loc l)
  (let ((t (location->tree l)))
    (location (move-to-left t)
	      (push-context-element (left (current-element t)
					  (move-to-right t))
				    (location->context l)))))

(define (move-to-right-loc l)
  (let ((t (location->tree l)))
    (location (move-to-right t)
	      (push-context-element (right (current-element t)
					   (move-to-left t))
				    (location->context l)))))

(define (at-root?-loc l) (root-context? (location->context l)))

(define (at-leaf?-loc l) (at-leaf? (location->tree l)))

(define (insert-to-left-loc n l)
  (location (insert-to-left n (location->tree l))
	    (location->context l)))

(define (insert-to-right-loc n l)
  (location (insert-to-right n (location->tree l))
	    (location->context l)))
