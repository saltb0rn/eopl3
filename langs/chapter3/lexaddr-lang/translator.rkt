#lang racket

(require eopl
         "lang.rkt")

(provide translation-of-program)

;;;;;;;;;;;;;;;;;; translator to translate AST

;; Nameless-program is Program without any varables

;; translation-of-program : Program -> Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
           [a-program (exp1)
                      (a-program
                       (translation-of exp1 (init-senv)))])))

;; translation-of : Exp * Senv -> Nameless-exp
(define translation-of
  (lambda (exp senv)
    (cases expression exp
           [const-exp (num) (const-exp num)]

           [let-exp (var exp1 exp2)
                    (nameless-let-exp
                     (translation-of exp1 (extend-senv var senv))
                     (translation-of exp2 (extend-senv var senv)))]

           [var-exp (var) (nameless-var-exp
                           (apply-senv senv var))]

           [proc-exp (var body)
                     (nameless-proc-exp
                      (translation-of body (extend-senv var senv)))]

           [diff-exp (exp1 exp2)
                     (diff-exp
                      (translation-of exp1 senv)
                      (translation-of exp2 senv))]

           [if-exp (exp1 exp2 exp3)
                   (if-exp
                    (translation-of exp1 senv)
                    (translation-of exp2 senv)
                    (translation-of exp3 senv))]

           [call-exp (exp1 exp2)
                     (call-exp
                      (translation-of exp1 senv)
                      (translation-of exp2 senv))]

           [else (report-invalid-source-expression)])))

(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error 'value-of
                "Illegal expression in source code: ~s" exp)))

;;;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;;;

;;; Senv = Listof(Sym)
;;; Lexaddr = N

;; empty-senv : () -> Senv
(define empty-senv
  (lambda () '()))

;; extend-senv : Var * Senv -> Senv
(define extend-senv
  (lambda (var senv)
    (cons var senv)))

;; apply-senv : Senv * Var -> Lexaddr
(define apply-senv
  (lambda (senv var)
    (cond
     [(null? senv) (report-unbound-var var)]
     [(eqv? var (car senv)) 0]
     [else
      (+ 1 (apply-senv (cdr senv) var))])))

(define report-unbound-var
  (lambda (var)
    (eopl:error 'translation-of "unbound variable in code: ~s" var)))

;; init-env : () -> Senv
(define init-senv
  (lambda ()
    (extend-senv 'i
                 (extend-senv 'v
                              (extend-senv 'x
                                           (empty-senv))))))
