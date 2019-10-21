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

;; cps-of-exps: Listof(InpExp) * (Listof(InpExp) -> TfExp) -> TfExp
;; usage:
;;   -- assume e_i's are non-simple, b_i's are simple
;;   -- then
;;        (cps-of-exps '(b1 b2 e1 b3 e2 e3) F) ==
;;        [e1](\v1.[e2](\v2.[e3](\v3.(F `(,<b1> ,<b2> ,v1 ,<b3> ,v2 ,v3)))))
;;      where <b> is cps-of-simple-exp of b.
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
                            (list-set exps pos (var-exp var))))))))))


;; inp-exp-simple? : InpExp -> Bool
;; returns #t or #f, depending on whether exp would be a
;; simple-exp if reparsed using the CPS-OUT language.
(define (inp-exp-simple? exp)
  (cases expression exp
         [const-exp (num) #t]
         [var-exp (var) #t]
         [diff-exp (exp1 exp2)
                   (and
                    (inp-exp-simple? exp1)
                    (inp-exp-simple? exp2))]
         [zero?-exp (exp1)
                    (inp-exp-simple? exp1)]
         [proc-exp (ids exp) #t]
         [sum-exp (exps)
                  (all-simple? exps)]
         [else #f]))

(define (all-simple? exps)
  (if (null? exps)
      #t
      (and (inp-exp-simple? (car exps))
           (all-simple? (cdr exps)))))


(define (cps-of-simple-exp exp)
  (cases expression exp
         [const-exp (num) (cps-const-exp num)]
         [var-exp (var) (cps-var-exp var)]
         [diff-exp (exp1 exp2)
                   (cps-diff-exp
                    (cps-of-simple-exp exp1)
                    (cps-of-simple-exp exp2))]
         [zero?-exp (exp1)
                    (cps-zero?-exp
                     (cps-of-simple-exp exp1))]
         [proc-exp (ids exp)
                   (cps-proc-exp (append ids (list 'k%00))
                                 (cps-of-exp exp (cps-var-exp 'k%00)))]
         [sum-exp (exps)
                  (cps-sum-exp
                   (map cps-of-simple-exp exps))]
         [else (report-invalid-exp-to-cps-of-simple-exp exp)]))

(define report-invalid-exp-to-cps-of-simple-exp
    (lambda (exp)
      (eopl:error 'cps-simple-of-exp
        "non-simple expression to cps-of-simple-exp: ~s"
        exp)))


(define (cps-of-zero?-exp exp1 k-exp)
  (cps-of-exps (list exp1)
               (lambda (new-rands)
                 (make-send-to-cont
                  k-exp
                  (cps-zero?-exp
                   (car new-rands))))))


(define (cps-of-sum-exp exps k-exp)
  (cps-of-exps exps
               (lambda (new-rands)
                 (make-send-to-cont
                  k-exp
                  (cps-sum-exp new-rands)))))

(define (cps-of-diff-exp exp1 exp2 k-exp)
  (cps-of-exps (list exp1 exp2)
               (lambda (new-rands)
                 (make-send-to-cont
                  k-exp
                  (cps-diff-exp
                   (car new-rands)
                   (cadr new-rands))))))

(define (cps-of-if-exp exp1 exp2 exp3 k-exp)
  (cps-of-exps (list exp1 exp2 exp3)
               (lambda (new-rands)
                 (cps-if-exp (car new-rands)
                             (cps-of-exp exp2 k-exp)
                             (cps-of-exp exp3 k-exp)))))


;; This is wrong, because it puts k-exp inside the scope of id.
;; cps-of-let-exp : Var * InpExp * InpExp * SimpleExp -> TfExp
;; Page: 222
;; (define cps-of-let-exp
;;   (lambda (id rhs body k-exp)
;;     (cps-of-exps (list rhs)
;;       (lambda (new-rands)
;;         (cps-let-exp id 
;;           (car new-rands)
;;           (cps-of-exp body k-exp))))))

;; same for this one
;; (define cps-of-let-exp
;;   (lambda (id rhs body k-exp)
;;     (cps-of-exp rhs
;;                 (cps-proc-exp
;;                  (list id)
;;                  (cps-of-exp body k-exp)))))

;; Thanks to github user EFanZh for discovering this bug.

;; Correct solution: translate the 'let' into the immediate
;; application of a lambda expression.  Then
;; cps-of-exps will make the needed fresh variables

(define (cps-of-let-exp id rhs body k-exp)
  (cps-of-exp
   (call-exp
    (proc-exp (list id) body)
    (list rhs))
   k-exp))


;; cps-of-letrec-exp: Listof(Var) * Listof(Listof(Var)) * Listof(InpExp) * InpExp * SimpleExp -> TfExp
(define (cps-of-letrec-exp proc-names idss proc-bodies body k-exp)
  (cps-letrec-exp
   proc-names
   (map
    (lambda (ids) (append ids (list 'k%00)))
    idss)
   (map
    (lambda (exp) (cps-of-exp exp (cps-var-exp 'k%00)))
    proc-bodies)
   (cps-of-exp body k-exp)))

;; cps-of-call-exp : InpExp * Listof(InpExp) * SimpleExp -> TfExp
(define (cps-of-call-exp rator rands k-exp)
  (cps-of-exps (cons rator rands)
               (lambda (new-rands)
                 (cps-call-exp
                  (car new-rands)
                  (append (car new-rands) (list k-exp))))))



(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"
        (number->string sn))))))

;; list-set : SchemeList * Int * SchemeVal -> SchemeList
;; returns a list lst1 that is just like lst, except that
;; (listref lst1 n) = val.
(define (list-set lst n val)
  (cond
   [(null? lst) (eopl:error 'list-set "ran off end")]
   [(zero? n) (cons val (cdr lst))]
   [else (cons (car lst) (list-set (cdr lst) (- n 1) val))]))

;; list-index : (SchemeVal -> Bool) * SchemeList -> Maybe(Int)
;; returns the smallest number n such that (pred (listref lst n))
;; is true.  If pred is false on every element of lst, then returns
;; #f.
(define (list-index pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) 0]
   [(list-index pred (cdr lst)) => (lambda (n) (+ n 1))]
   [else #f]))
