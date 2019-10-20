#lang racket

(require "cps-in-lang.rkt"
         "cps-out-lang.rkt"
         eopl)
(provide cps-of-program)


;; cps-of-program : InpExp -> TfExp
(define cps-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (cps-a-program
                       (cps-of-exps (list exp1)
                                    (lambda (new-args)
                                      (simple-exp->exp (car new-args)))))))))
                       

;; cps-of-exp : Exp * SimpleExp -> TfExp
(define (cps-of-exp exp cont)
  (cases expression exp
         [const-exp (num) (make-send-to-cont cont (cps-const-exp num))]
         [var-exp (var) (make-send-to-cont cont (cps-var-exp var))]
         [proc-exp (vars body)
                   (make-send-to-cont cont
                                      (cps-proc-exp (append vars (list 'k%00))
                                                    (cps-of-exp body (cps-var-exp 'k%00))))]
         [zero?-exp (exp1)
                    (cps-of-zero?-exp exp1 cont)]
         [diff-exp (exp1 exp2)
                   (cps-of-diff-exp exp1 exp2 cont)]
         [sum-exp (exps)
                  (cps-of-sum-exp exps cont)]
         [if-exp (exp1 exp2 exp3)
                 (cps-of-if-exp exp1 exp2 exp3 cont)]
         [let-exp (var exp1 body)
                  (cps-of-let-exp var exp1 body cont)]
         [letrec-exp (ids bidss proc-bodies body)
                     (cps-of-letrec-exp ids bidss proc-bodies body cont)]
         [call-exp (rator rands)
                   (cps-of-call-exp rator rands cont)]))

(define (make-send-to-cont cont bexp)
  (cps-call-exp cont (list bexp)))


(define (cps-of-exps exps builder)
  (let cps-of-rest ((exps exps))
    ;; cps-of-rest : Listof(InpExp) -> TfExp
    (let ((pos (list-index
                (lambda (exp)
                  (not (inp-exp-simple? exp)))
                exps)))
      (if (not pos)
          (builder (map cps-of-simple-exp exps))
          (let ((var (fresh-identifier 'var)))
            (cps-of-exp
             (list-ref exps pos)
             (cps-proc-exp (list var)
                           (cps-of-rest
                            (list-set exps pos (var-epx var))))))))))


(define (cps-of-zero?-exp exp1 k-exp)
  (cps-of-exps (list exp1)
               (lambda (new-rands)
                 (make-send-to-cont
                  k-exp
                  (cps-zero?-exp
                   (car new-rands))))))
  
