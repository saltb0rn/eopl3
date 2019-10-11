#lang racket

(require eopl
         "lang.rkt")

;;;;;;;;;;;;;;;;;; Values ;;;;;;;;;;;;;;;;;;

;;; denoted values and expressed values are identical.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?)))

;; extractors:

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
           (num-val (num) num)
           (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
           (bool-val (bool) bool)
           (else (expval-extractor-error 'bool v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;
(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

(define env?
  (lambda (x)
    (or (empty-env? x)
        (and [pair? x]
             [symbol? (car (car x))]
             [expval? (cadr (car x))]
             [env? (cdr x)]))))

(define empty-env (lambda () '()))

(define empty-env? null?)

(define extend-env
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ([sym (car (car env))]
              [val (cadr (car env))]
              [old-env (cdr env)])
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))


;;;;;;;;;;;;;;;;;; the interpreter, observers ;;;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           [const-exp (num) (num-val num)]
           [var-exp (var) (apply-env env var)]
           [diff-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val2)))
                         (num-val
                          (- num1 num2))))]
           [zero?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (let ((num1 (expval->num val1)))
                          (if (zero? num1)
                              (bool-val #t)
                              (bool-val #f))))]
           [if-exp (exp1 exp2 exp3)
                   (let ((val1 (value-of exp1 env)))
                     (if (expval->bool val1)
                         (value-of exp2 env)
                         (value-of exp3 env)))]
           [let-exp (var exp1 body)
                    (let ((val1 (value-of exp1 env)))
                      (value-of body
                                (extend-env var val1 env)))])))


;;;;;;;;;;;;;;;;;; Example ;;;;;;;;;;;;;;;;;;

(define run
  (lambda (src)
    (cases expval (value-of-program (scan&parse src))
           [num-val (num) num]
           [bool-val (bool) bool]
           [else (eopl:error 'expval-extractors "Undefined")])))

(run "if zero?(0) then 2 else 0") ; => 2
(run "-(1,2)")                    ; => -1
(run "let a = 1 in -(10, a)")     ; => 9
(run "i")
