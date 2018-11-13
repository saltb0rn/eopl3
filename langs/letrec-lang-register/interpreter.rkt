#lang racket

(require eopl
         "lang.rkt")

;;;;;;;;;;;;;;;;;; Register ;;;;;;;;;;;;;;;;;;
(define reg-exp 'uninitialized)
(define reg-env 'uninitialized)
(define reg-cont 'uninitialized)
(define reg-val 'uninitialized)
(define reg-proc 'uninitialized)

;;;;;;;;;;;;;;;;;; Values ;;;;;;;;;;;;;;;;;;
(define-datatype expval expval?
  [num-val (value number?)]
  [bool-val (value boolean?)]
  [proc-val (value proc?)])

(define proc? procedure?)

(define procedure
  (lambda (var body saved-env)
    (lambda (val cont)
      (set! reg-cont cont)
      (set! reg-env (extend-env var val saved-env))
      (set! reg-exp body)
      (value-of/k))))

;; apply-procedure : Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda ()
    (reg-proc reg-val reg-cont)))

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
  (lambda ()
    (cases continuation reg-cont
           [end-cont ()
                     (begin
                       (eopl:printf "End of computation.~%")
                       reg-val)]
           [zero1-cont (saved-cont)
                       (set! reg-cont saved-cont)
                       (set! reg-val (bool-val (zero? (expval->num reg-val))))
                       (apply-cont)]
           [let-exp-cont (var body saved-env saved-cont)
                         (set! reg-env (extend-env var reg-val saved-env))
                         (set! reg-cont saved-cont)
                         (set! reg-exp body)
                         (value-of/k)]
           [if-test-cont (exp1 exp2 saved-env saved-cont)
                           (set! reg-exp (if (expval->bool reg-val) exp1 exp2))
                           (set! reg-env saved-env)
                           (set! reg-cont saved-cont)
                           (value-of/k)]
           [diff1-cont (exp2 saved-env saved-cont)
                       (set! reg-env saved-env)
                       (set! reg-cont (diff2-cont reg-val saved-cont))
                       (set! reg-exp exp2)
                       (value-of/k)]
           [diff2-cont (val1 saved-cont)
                       (let ([num1 (expval->num val1)]
                             [num2 (expval->num reg-val)])
                         (set! reg-val (num-val (- num1 num2)))
                         (set! reg-cont saved-cont)
                         (apply-cont))]
           [rator-cont (rand saved-env saved-cont)
                       (set! reg-env saved-env)
                       (set! reg-cont (rand-cont reg-val saved-cont))
                       (set! reg-exp rand)
                       (value-of/k)]
           [rand-cont (val1 saved-cont)
                      (set! reg-proc (expval->proc val1))
                      (set! reg-cont saved-cont)
                      (set! reg-val reg-val)
                      (apply-procedure/k)]
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
           (a-program (exp1)
                      (set! reg-exp exp1)
                      (set! reg-env (init-env))
                      (set! reg-cont (end-cont))
                      (value-of/k)))))

;; value-of : Exp * Env * Cont -> ExpVal
(define value-of/k
  (lambda ()
    (cases expression reg-exp
           [const-exp (num)
                      (set! reg-val (num-val num))
                      (apply-cont)]
           [var-exp (var)
                    (set! reg-val (apply-env reg-env var))
                    (apply-cont)]
           [diff-exp (exp1 exp2)
                     (set! reg-cont (diff1-cont exp2 reg-env reg-cont))
                     (set! reg-exp exp1)
                     (value-of/k)]
           [zero?-exp (exp1)
                      (set! reg-exp exp1)
                      (set! reg-cont (zero1-cont reg-cont))
                      (value-of/k)]
           [if-exp (exp1 exp2 exp3)
                   (set! reg-exp exp1)
                   (set! reg-cont (if-test-cont exp2 exp3 reg-env reg-cont))
                   (value-of/k)]
           [let-exp (var exp1 body)
                    (set! reg-exp exp1)
                    (set! reg-cont (let-exp-cont var body reg-env reg-cont))
                    (value-of/k)]
           [proc-exp (var body)
                     (set! reg-val (proc-val (procedure var body reg-env)))
                     (apply-cont)]
           [call-exp (rator rand)
                     (set! reg-exp rator)
                     (set! reg-cont (rator-cont rand reg-env reg-cont))
                     (value-of/k)]
           [letrec-exp (proc-name var proc-body letrec-body)
                       (let ((new-env (extend-env-rec proc-name var proc-body reg-env)))
                         (set! reg-env new-env)
                         (set! reg-exp letrec-body)
                         (value-of/k))]
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
