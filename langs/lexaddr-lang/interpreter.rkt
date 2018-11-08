#lang racket

(require eopl
         "lang.rkt"
         "translator.rkt")

;;;;;;;;;;;;;;;;;; Values ;;;;;;;;;;;;;;;;;;

(define-datatype expval expval?
  [num-val
   (value number?)]
  [bool-val
   (value boolean?)]
  [proc-val
   (value proc?)])

(define expval->num
  (lambda (v)
    (cases expval v
           [num-val (value) value]
           [else (expval-extractor-error 'number v)])))

(define expval->bool
  (lambda (v)
    (cases expval v
           [bool-val (value) value]
           [else (expval-extractor-error 'bool v)])))

(define expval->proc
  (lambda (v)
    (cases expval v
           [proc-val (value) value]
           [else (expval-extractor-error 'proc v)])))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;; proc? : SchemeVal -> Bool
(define proc? procedure?)

;; procedure : Exp * Nameless-env -> Proc
(define procedure
  (lambda (body senv)
    (lambda (arg)
      (value-of body (extend-nameless-env arg senv)))))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc arg)
    (proc arg)))

;;;;;;;;;;;;;;;;;; environment ;;;;;;;;;;;;;;;;;;
;; nameless-env? : SchemeVal -> Bool
(define nameless-env?
  (lambda (x)
    ((list-of expval?) x)))

;; empty-nameless-env? : Nameless-environment -> Bool
(define empty-nameless-env? null?)

;; empty-nameless-env : () -> Nameless-env
(define empty-nameless-env (lambda () '()))

;; extend-nameless-env : ExpVal * Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

;; apply-nameless-env : Nameless-env * Lexaddr -> ExpVal
(define apply-nameless-env
  (lambda (nameless-env lexaddr)
    (list-ref nameless-env lexaddr)))


;; init-nameless-env : () -> Nameless-env
(define init-nameless-env
  (lambda ()
    (extend-nameless-env
     (num-val 1)
     (extend-nameless-env
      (num-val 5)
      (extend-nameless-env
       (num-val 10)
       (empty-nameless-env))))))

;;;;;;;;;;;;;;;;;; interpreter ;;;;;;;;;;;;;;;;;;
;; value-of-translation : Nameless-program -> ExpVal
(define value-of-translation
  (lambda (pgm)
    (cases program pgm
           [a-program (exp1)
                      (value-of exp1 (init-nameless-env))])))

;; Nameless-exp is the Expression without any variables
;; value-of : Nameless-exp * Nameless-env -> ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
           [const-exp (num) (num-val num)]

           [diff-exp (exp1 exp2)
                     (let ([val1 (expval->num
                                  (value-of exp1 nameless-env))]
                           [val2 (expval->num
                                  (value-of exp2 nameless-env))])
                       (num-val (- val1 val2)))]

           [zero?-exp (exp1)
                      (bool-val
                       (zero? (expval->num (value-of exp1 nameless-env))))]

           [if-exp (exp0 exp1 exp2)
                   (let ([val0 (expval->bool (value-of exp0 nameless-env))])
                     (if val0
                         (value-of exp1 nameless-env)
                         (value-of exp2 nameless-env)))]

           [call-exp (rator rand)
                     (let ([proc (expval->proc (value-of rator nameless-env))]
                           [arg (value-of rand nameless-env)])
                       (apply-procedure proc arg))]

           [nameless-var-exp (lexaddr)
                             (apply-nameless-env nameless-env lexaddr)]

           [nameless-let-exp (exp1 exp2)
                             (let ([val (value-of exp1 nameless-env)])
                               (value-of exp2
                                         (extend-nameless-env val nameless-env)))]

           [nameless-proc-exp (body)
                              (proc-val (procedure body nameless-env))]

           [else
            (eopl:error 'value-of
                        "Illgeal expression in translated code: ~s" exp)])))

;;;;;;;;;;;;;;;;;; example ;;;;;;;;;;;;;;;;;;

(define run
  (lambda (src)
    (let ((res (value-of-translation
                (translation-of-program
                 (scan&parse src)))))
      (cases expval res
             [num-val (value) value]
             [bool-val (value) value]
             [proc-val (value) value]))))

(run "(proc (a) -(1,a) 4)")
