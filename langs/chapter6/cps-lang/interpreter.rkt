#lang racket

(require eopl
         "cps-out-lang.rkt")

;; (provide value-of-program
;;          value-of/k)

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  [num-val
   (value number?)]
  [bool-val
   (boolean boolean?)]
  [proc-val
   (proc proc?)])

(define-datatype proc proc?
  [procedure
   (vars (list-of symbol?))
   (body tfexp?)
   (env environment?)])

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;;; represent environment as a list of bindings.
;;; binding ::= ('let    (list-of id) (list-of expval)) 
;;;           | ('letrec (list-of id) (list-of bvar) (list-of expression))

;;; The first binding for extend-env*, the second is for
;;; extend-env-rec**. 

;;; this representation is designed to make the printed representation
;;; of the environment more readable.
(define (empty-env)
  '())

(define (empty-env? env)
  (null? env))

(define (extend-env* syms vals old-env)
  (cons (list 'let syms vals) old-env))

(define (extend-env-rec** p-names b-varss p-bodies saved-env)
  (cons
   (list 'letrec p-names b-varss p-bodies)
   saved-env))

;; no precise, but will do.
(define environment?
  (list-of
   (lambda (p)
     (and
      (pair? p)
      (or (eqv? (car p) 'let) (eqv? (car p) 'letrec))))))


;;; extractors:

(define (expval->num v)
  (cases expval v
         [num-val (num) num]
         [else (expval-extractor-error 'num v)]))

(define (expval->bool v)
  (cases expval v
         [bool-val (bool) bool]
         [else (expval-extractor-error 'bool v)]))

(define (expval->proc v)
  (cases expval v
         [proc-val (proc) proc]
         [else (expval-extractor-error 'proc v)]))

(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
              variant value))
