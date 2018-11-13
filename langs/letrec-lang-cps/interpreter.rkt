#lang racket

(require eopl
         "lang.rkt")

;;;;;;;;;;;;;;;;;; Values ;;;;;;;;;;;;;;;;;;
(define-datatype expval expval?
  [num-val (value number?)]
  [bool-val (value boolean?)]
  [proc-val (value proc?)])

(define proc? procedure?)

(define procedure
  (lambda (var body saved-env)
    (lambda (val cont)
      (value-of/k body (extend-env var val saved-env) cont))))

;; apply-procedure : Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 val1 cont)
    (proc1 val1 cont)))

(define expval->num
  (lambda (v)
    (cases expval v
           [num-val (num) num]
           [else (expval-extractor-error 'num v)])))

(define expval->bool
  (lambda (v)
    (cases expval v
           [bool-val (bool) bool]
           [else (expval-extractor-error 'bool v)])))

(define expval->proc
  (lambda (v)
    (cases expval v
           [proc-val (proc) proc]
           [else (expval-extractor-error 'proc v)])))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;; Continuation
(define-datatype continuation continuation?
  [end-cont]
  [zero1-cont
   (saved-cont continuation?)]
  [let-exp-cont
   (var identifier?)
   (body expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [if-test-cont
   (exp1 expression?)
   (exp2 expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [diff1-cont
   (exp2 expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [diff2-cont
   (val expval?)
   (saved-cont continuation?)]
  [rator-cont
   (rand expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [rand-cont
   (val1 expval?)
   (saved-cont continuation?)])

;; apply-cont : Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           [end-cont ()
                     (begin
                       (eopl:printf "End of computation.~%")
                       val)]
           [zero1-cont (saved-cont)
                       (apply-cont saved-cont
                                   (bool-val
                                    (zero? (expval->num val))))]
           [let-exp-cont (var body saved-env saved-cont)
                         (value-of/k
                          body
                          (extend-env var val saved-env)
                          saved-cont)]
           [if-test-cont (exp1 exp2 saved-env saved-cont)
                         (if (expval->bool val)
                             (value-of/k exp1 saved-env saved-cont)
                             (value-of/k exp2 saved-env saved-cont))]
           [diff1-cont (exp2 saved-env saved-cont)
                       (value-of/k exp2 saved-env (diff2-cont val saved-cont))]
           [diff2-cont (val1 saved-cont)
                       (let ([num1 (expval->num val1)]
                             [num2 (expval->num val)])
                         (apply-cont saved-cont (num-val (- num1 num2))))]
           [rator-cont (rand saved-env saved-cont)
                       (value-of/k rand saved-env
                                   (rand-cont val saved-cont))]
           [rand-cont (val1 saved-cont)
                      (apply-procedure/k
                       (expval->proc val1)
                       val saved-cont)]
           )))


;;;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;

(define-datatype env env?
  [empty-env]
  [extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env env?)]
  [extend-env-rec
   (p-name symbol?)
   (b-var symbol?)
   (p-body expression?)
   (saved-env env?)])

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

;; apply-env : Env * Symbol -> ExpVal
(define apply-env
  (lambda (env1 search-sym)
    (cases env env1
           [empty-env ()
                      (eopl:error 'apply-env "No binding for ~s" search-sym)]
           [extend-env (var val saved-env)
                       (if (eqv? search-sym var)
                           val
                           (apply-env saved-env search-sym))]
           [extend-env-rec (p-name b-var p-body saved-env)
                           (if (eqv? search-sym p-name)
                               (proc-val (procedure b-var p-body env1))
                               (apply-env saved-env search-sym))])))


;;;;;;;;;;;;;;;;;; the interpreter, observers ;;;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1) (value-of/k exp1 (init-env) (end-cont))))))

;; value-of : Exp * Env * Cont -> ExpVal
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
           [const-exp (num) (apply-cont cont (num-val num))]
           [var-exp (var) (apply-cont cont (apply-env env var))]
           [diff-exp (exp1 exp2)
                     (value-of/k exp1 env
                                 (diff1-cont exp2 env cont))]
           [zero?-exp (exp1)
                      (value-of/k exp1 env (zero1-cont cont))]
           [if-exp (exp1 exp2 exp3)
                   (value-of/k
                    exp1
                    env
                    (if-test-cont exp2 exp3 env cont))]
           [let-exp (var exp1 body)
                    (value-of/k
                     exp1
                     env
                     (let-exp-cont var body env cont))]
           [proc-exp (var body)
                     (apply-cont
                      cont
                      (proc-val (procedure var body env)))]
           [call-exp (rator rand)
                     (value-of/k
                      rator
                      env
                      (rator-cont rand env cont))]
           [letrec-exp (proc-name var proc-body letrec-body)
                       (let ((new-env (extend-env-rec proc-name var proc-body env)))
                         (value-of/k letrec-body new-env cont))]
           )))

;;;;;;;;;;;;;;;;;; Example ;;;;;;;;;;;;;;;;;;

(define run
  (lambda (src)
    (let ((res (value-of-program (scan&parse src))))
      (cases expval res
             [num-val (v) v]
             [bool-val (v) v]
             [proc-val (v) v]))))


(run "letrec p (z) = if zero?(z) then 1 else (p -((p -(z,1)),1)) in (p 11)")
