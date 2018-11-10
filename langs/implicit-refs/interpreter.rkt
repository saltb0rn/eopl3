#lang racket

(require eopl
         "lang.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;; Values ;;;;;;;;;;;;;;;;;;
(define-datatype expval expval?
  [num-val
   (value number?)]
  [bool-val
   (value boolean?)]
  [proc-val
   (value proc?)]
  [ref-val
   (value reference?)])

(define expval->num
  (lambda (v)
    (cases expval v
           (num-val (num) num)
           (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
           (bool-val (bool) bool)
           (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
           (proc-val (proc) proc)
           (else (expval-extractor-error 'proc v)))))

(define expval->ref
  (lambda (v)
    (cases expval v
           (ref-val (ref) ref)
           (else (expval-extractor-error 'reference v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;; Proc
(define-datatype proc proc?
  [procedure
   (var symbol?)
   (body expression?)
   (saved-env environment?)])

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
           [procedure (var body saved-env)
                      (value-of body (extend-env var (newref arg) saved-env))])))

;;; Store
(define the-store 'uninitialized)

;; init-store! : () -> Store
(define init-store!
  (lambda () (set! the-store (empty-store))))

;; empty-store : () -> Store
(define empty-store
  (lambda () '()))

;; get-store : () -> Store
(define get-store
  (lambda () the-store))

;; reference? : SchemeVal -> Bool
(define reference? integer?)

;; newref : ExpVal -> Ref
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store
            (append the-store (list val)))
      next-ref)))

;; deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (list-ref the-store ref)))

;; setref! : Ref * ExpVal -> Unspecified
(define setref!
  (lambda (ref val)
    (set! the-store
          (let setref-inner ([store1 the-store] [ref1 ref])
            (cond
             [(null? store1)
              (report-invalid-reference ref the-store)]
             [(zero? ref1)
              (cons val (cdr store1))]
             [else
              (cons
               (car store1)
               (setref-inner
                (cdr store1) (- ref1 1)))])))))

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
   (ref reference?)
   (old-env environment?)]
  [extend-env-rec*
   (proc-names
    (list-of symbol?))
   (vars
    (list-of symbol?))
   (proc-bodies
    (list-of expression?))
   (saved-env environment?)])

;; init-env : () -> Env
(define init-env
  (lambda ()
    (extend-env
     'i (newref (num-val 1))
     (extend-env
      'v (newref (num-val 5))
      (extend-env
       'x (newref (num-val 10))
       (empty-env))))))

;; apply-env : Env * Search-Sym -> Ref
(define apply-env
  (lambda (env search-sym)
    (cases environment env
           [empty-env ()
                      (eopl:error 'apply-env "No binding for ~s" search-sym)]
           [extend-env (var ref old-env)
                       (if (eqv? var search-sym)
                           ref
                           (apply-env old-env search-sym))]
           [extend-env-rec* (p-names vars p-bodies saved-env)
                            (cond
                             [(letrec-index search-sym p-names saved-env)
                              => (lambda (n)
                                   (procedure
                                    (list-ref vars n)
                                    (list-ref p-bodies n)
                                    env))]
                             [else (apply-env saved-env search-sym)])])))

;; letrec-index : Search-Sym * Listof(Sym) -> Maybe(Int)
(define letrec-index
  (lambda (search-sym syms)
    (cond
     [(null? syms) #f]
     [(eqv? search-sym (car syms)) 0]
     [(letrec-index search-sym (cdr syms))
      => (lambda (n) (+ n 1))]
     [else #f])))


;;;;;;;;;;;;;;;;;; interpreter ;;;;;;;;;;;;;;;;;;
;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (init-store!)
    (cases program pgm
           [a-program (exp1)
                      (value-of exp1 (init-env))])))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           [const-exp (num) (num-val num)]
           [var-exp (var) (deref (apply-env env var))]
           [diff-exp (exp1 exp2)
                     (num-val
                      (- (expval->num (value-of exp1 env))
                         (expval->num (value-of exp2 env))))]
           [zero?-exp (exp1)
                      (bool-val
                       (expval->bool
                        (zero? (expval->num (value-of exp1 env)))))]
           [if-exp (exp0 exp1 exp2)
                   (if (expval->bool (value-of exp0 env))
                       (value-of exp1 env)
                       (value-of exp2 env))]
           [proc-exp (var body)
                     (proc-val (procedure var body env))]
           [call-exp (rator rand)
                     (let ([proc1 (expval->proc (value-of rator env))]
                           [arg (value-of rand env)])
                       (apply-procedure proc1 arg))]
           [let-exp (var exp1 exp2)
                    (value-of exp2
                              (extend-env var (newref (value-of exp1 env)) env))]
           [letrec-exp (p-names vars p-bodies letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec* p-names vars p-bodies env))]
           [begin-exp (exp1 exps)
                      (let value-of-begins ([e1 exp1] [es exps])
                        (if (null? es)
                            (value-of e1 env)
                            (value-of-begins (car es) (cdr es))))]
           [assign-exp (var exp1)
                       (begin
                         (setref!
                          (apply-env env var)
                          (value-of exp1 env))
                         (num-val 27))])))


;;;;;;;;;;;;;;;;;; example ;;;;;;;;;;;;;;;;;;
(define run
  (lambda (src)
    (value-of-program
     (scan&parse src))))

(run
"let f = proc (x) proc (y)
          begin
           set x = -(x,-1);
           -(x,y)
          end
in ((f 44) 33)")
