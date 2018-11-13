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
    (lambda (val)
      (value-of body (extend-env var val saved-env)))))

;; apply-procedure : Proc x ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val1)
    (proc1 val1)))

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

;;;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;

;; init-env : '() -> Env
(define init-env
  (lambda ()
    [extend-env
     'i (num-val 1)
     [extend-env
      'v (num-val 5)
      [extend-env
       'x (num-val 10)
       [empty-env]]]]))

;; env? : Env -> bool
(define env?
  (lambda (x)
    (or [empty-env? x]
        (and
         [pair? x]
         [symbol? (car (car x))]
         [expval? (cadar x)]
         [env? (cdr x)]))))

;; empty-env : '() -> EmptyEnv
(define empty-env (lambda () '()))

;; empty-env? : Env -> bool
(define empty-env? null?)

;; extend-env : Var x ExpVal x Env -> Env
(define extend-env
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

;; extend-env-rec : ProcName x BoundVar x ProcBody x LetrecBody -> Env
(define extend-env-rec
  (lambda (proc-name bound-var proc-body old-env)
    (let ((vec (make-vector 1)))
      (let ((new-env (extend-env proc-name vec old-env)))
        (vector-set! vec 0
                     (proc-val (procedure bound-var proc-body new-env)))
        new-env))))

;; apply-env : Env x Var -> ExpVal
(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ([sym (caar env)]
              [val (cadar env)] ;; (car (cdr (car env)))
              [old-env (cdr env)])
          (if (eqv? search-sym sym)
              (if (vector? val)
                  (vector-ref val 0)
                  val)
              (apply-env old-env search-sym))))))

;;;;;;;;;;;;;;;;;; the interpreter, observers ;;;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1) (value-of exp1 (init-env))))))

;; value-of : Exp x Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           [const-exp (num) (num-val num)]
           [var-exp (var) (apply-env env var)]
           [diff-exp (exp1 exp2)
                     (let ([val1 (value-of exp1 env)]
                           [val2 (value-of exp2 env)])
                       (num-val
                        (- (expval->num val1)
                           (expval->num val2))))]
           [zero?-exp (exp1)
                      (let ([val1 (value-of exp1 env)])
                        (bool-val (zero? (expval->num val1))))]
           [if-exp (exp1 exp2 exp3)
                   (let ([val1 (value-of exp1 env)])
                     (if (expval->bool val1)
                         (value-of exp2 env)
                         (value-of exp3 env)))]
           [let-exp (var exp1 body)
                    (let ([val1 (value-of exp1 env)])
                      (value-of body
                                (extend-env var val1 env)))]
           [proc-exp (var body)
                     (proc-val (procedure var body env))]
           [call-exp (rator rand)
                     (let ([proc (expval->proc (value-of rator env))]
                           [arg (value-of rand env)])
                       (apply-procedure proc arg))]
           [letrec-exp (proc-name var proc-body letrec-body)
                       (let ((new-env (extend-env-rec proc-name var proc-body env)))
                         (value-of letrec-body new-env))]
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
