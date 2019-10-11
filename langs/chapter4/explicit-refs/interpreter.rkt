#lang racket

(require eopl
         "lang.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;; Values ;;;;;;;;;;;;;;;;;;

;;; ExpVal
(define-datatype expval expval?
  [num-val (value number?)]
  [bool-val (value boolean?)]
  [proc-val (value proc?)]
  [ref-val (value reference?)])


;; expval->num : ExpVal -> number?
(define expval->num
  (lambda (v)
    (cases expval v
           [num-val (value) value]
           [else (expval-extractor-error 'num v)])))

;; expval->bool : ExpVal -> boolean?
(define expval->bool
  (lambda (v)
    (cases expval v
           [bool-val (value) value]
           [else (expval-extractor-error 'bool v)])))

;; expval->proc : ExpVal -> proc?
(define expval->proc
  (lambda (v)
    (cases expval v
           [proc-val (value) value]
           [else (expval-extractor-error 'proc v)])))

;; expval->ref : ExpVal -> reference?
(define expval->ref
  (lambda (v)
    (cases expval v
           [ref-val (value) value]
           [else (expval-extractor-error 'reference v)])))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;; Proc
(define-datatype proc proc?
  [procedure
   (var symbol?)
   (body expression?)
   (env environment?)])

(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
           [procedure (var body saved-env)
                      (value-of body (extend-env var saved-env))])))

;;; Store

;; Store : a list: (ExpVal1 ExpVal2 ...) whose indics refer to ExpVals

;; the-store : a list to containt the current state of store.
(define the-store 'uninitialized)

;; init-store! : () -> Store
(define init-store! (lambda () (set! the-store (empty-store))))

;; emtpy-store : () -> Store
(define empty-store (lambda () '()))

;; get-store : () -> Store
(define get-store (lambda () the-store))

;; reference? : SchemeVal -> Bool
(define reference? integer?)

;; newref : ExpVal -> Ref
(define newref
  (lambda (v)
    (let ((new-ref (length the-store)))
      (set! the-store
            (append the-store (list v)))
      new-ref)))

;; deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (list-ref the-store ref)))

;; setref! : Ref * ExpVal -> Unspecified
(define setref!
  (lambda (ref val)
    (let setref-inner ([store1 the-store] [ref1 ref])
      (cond
       [(null? store1)
        (report-invalid-reference ref the-store)]
       [(zero? ref1)
        (cons val (cdr store1))]
       [else
        (cons
         (car store1)
         (setref-inner (cdr store1) (- ref1 1)))]))))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))

;;;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  [empty-env]
  [extend-env
   (var symbol?)
   (val expval?)
   (old-env environment?)]
  [extend-env-rec*
   (proc-names (list-of symbol?))
   (vars (list-of symbol?))
   (proc-bodies (list-of expression?))
   (saved-env environment?)])

;; init-env : () -> Env
(define init-env
  (lambda ()
    [extend-env
     'i (num-val 1)
     [extend-env
      'v (num-val 5)
      [extend-env
       'x (num-val 10)
       [empty-env]]]]))

;; apply-env : Env * SearchSym -> ExpVal
(define apply-env
  (lambda (env search-sym)
    (cases environment env
           [empty-env () (eopl:error 'apply-env "No binding for ~s" search-sym)]
           [extend-env (var val old-env)
                       (if (eqv? search-sym var)
                           val
                           (apply-env old-env search-sym))]
           [extend-env-rec* (p-names vars p-bodies saved-env)
                            (cond
                             [(letrec-index search-sym p-names)
                              => (lambda (n)
                                   (proc-val
                                    (procedure
                                     (list-ref vars n)
                                     (list-ref p-bodies n)
                                     env)))]
                             [else (apply-env saved-env search-sym)])])))

;; letrec-index : Sym * Listof(Sym) -> Maybe(Int)
(define letrec-index
  (lambda (search-sym syms)
    (cond
     [(null? syms) #f]
     [(eqv? search-sym (car syms)) 0]
     [(letrec-index search-sym (cdr syms))
      => (lambda (n) (+ n 1))]
     [else #f])))

;;;;;;;;;;;;;;;;;; Interpreter ;;;;;;;;;;;;;;;;;;
;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (init-store!)
    (cases program pgm
           [a-program (exp1)
                      (value-of exp1 (init-env))])))

;; value-of : Exp -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           [const-exp (num) (num-val num)]

           [var-exp (var) (apply-env env var)]

           [diff-exp (exp1 exp2)
                     (num-val
                      (- (expval->num (value-of exp1 env))
                         (expval->num (value-of exp2 env))))]

           [zero?-exp (exp1)
                      (expval->bool
                       (zero? (expval->num (value-of exp1 env))))]

           [if-exp (exp1 exp2 exp3)
                   (if (expval->bool (value-of exp1 env))
                       (value-of exp2 env)
                       (value-of exp3 env))]

           [let-exp (var exp1 body)
                    (value-of body (extend-env var (value-of exp1 env) env))]

           [proc-exp (var body)
                     (proc-val (procedure var body env))]

           [call-exp (rator rand)
                     (let ([proc (expval->proc (value-of rator env))]
                           [arg (value-of rand env)])
                       (apply-procedure proc arg))]

           [letrec-exp (p-names b-vars p-bodies letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec* p-names b-vars p-bodies env))]

           [begin-exp (exp1 exps)
                      (letrec
                          ([value-of-begins
                            (lambda (e1 es)
                              (let ([v1 (value-of e1 env)])
                                (if (null? es)
                                    v1
                                    (value-of-begins (car es) (cdr es)))))])
                        (value-of-begins exp1 exps))]

           [newref-exp (exp1)
                       (let ([v1 (value-of exp1 env)])
                         (ref-val (newref v1)))]

           [deref-exp (exp1)
                      (let ([v1 (value-of exp1 env)])
                        (let ([ref1 (expval->ref v1)])
                          (deref ref1)))]

           [setref-exp (exp1 exp2)
                       (let ([ref (expval->ref (value-of exp1 env))]
                             [v2 (value-of exp2 env)])
                         (begin
                           (setref! ref v2)
                           (num-val '23)))])))

;;;;;;;;;;;;;;;;;; example ;;;;;;;;;;;;;;;;;;

(define run
  (lambda (src)
    (value-of-program (scan&parse src))))

(run
 "let x = newref(newref(0))
  in begin
      setref(deref(x), 11);
      deref(deref(x))
     end")
