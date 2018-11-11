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
   (value reference?)]
  [mutpair-val
   (value mutpair?)])

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

(define expval->mutpair
  (lambda (v)
    (cases expval v
           (mutpair-val (pair) pair)
           (else (expval-extractor-error 'mutpair v)))))

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

;; apply-procedure : Proc * Ref -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
           [procedure (var body saved-env)
                      (value-of body (extend-env var arg saved-env))])))

;; value-of-operand : Exp * Env -> Ref
(define value-of-operand
  (lambda (exp1 env)
    (cases expression exp1
           [var-exp (var) (apply-env env var)]
           [else (newref (a-thunk exp1 env))])))

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

;;; MutPair
;; mutpair? : SchemeVal -> Bool
(define mutpair?
  (lambda (v)
    (reference? v)))

;; make-pair : ExpVal * ExpVal -> MutPair
(define make-pair
  (lambda (val1 val2)
    (let ([ref1 (newref val1)])
      (let ([ref2 (newref val2)])
        ref1))))

;; left : MutPair -> ExpVal
(define left
  (lambda (pair)
    (deref pair)))

;; right : MutPair -> ExpVal
(define right
  (lambda (pair)
    (deref (+ 1 pair))))

;; setleft : MutPair * ExpVal -> Unspecified
(define setleft
  (lambda (pair val)
    (setref! pair val)))

;; setright : MutPair * ExpVal -> Unspecified
(define setright
  (lambda (pair val)
    (setref! (+ 1 pair) val)))


;;; Thunk
(define-datatype thunk thunk?
  (a-thunk
   (exp1 expression?)
   (env environment?)))

;; value-of-thunk : Thunk -> ExpVal
(define value-of-thunk
  (lambda (v)
    (cases thunk v
           [a-thunk (exp1 env)
                    (value-of exp1 env)])))

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
           [var-exp (var)
                    (let* ([ref1 (apply-env env var)]
                           [w (deref ref1)])
                      (if (expval? w)
                          w
                          (let ([v1 (value-of-thunk w)])
                            (begin
                              (setref! ref1 v1)
                              v1))))]
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
                           [arg (value-of-operand rand env)])
                       (apply-procedure proc1 arg))]
           [letrec-exp (p-names vars p-bodies letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec* p-names vars p-bodies env))]
           [let-exp (var exp1 body)
                    (let ((val (value-of exp1 env)))
                      (value-of body
                                (extend-env var (newref val) env)))]
           [begin-exp (exp1 exps)
                      (let value-of-begins ([e1 exp1] [es exps])
                        (let ((v1 (value-of e1 env)))
                        (if (null? es)
                            v1
                            (value-of-begins (car es) (cdr es)))))]
           [assign-exp (var exp1)
                       (begin
                         (setref!
                          (apply-env env var)
                          (value-of exp1 env))
                         (num-val 27))]

           [newpair-exp (exp1 exp2)
                        (let ([v1 (value-of exp1 env)]
                              [v2 (value-of exp2 env)])
                          (mutpair-val (make-pair v1 v2)))]

           [left-exp (exp1)
                     (let* ([v1 (value-of exp1 env)]
                            [p1 (expval->mutpair v1)])
                       (left p1))]

           [right-exp (exp1)
                      (let* ([v1 (value-of exp1 env)]
                             [p1 (expval->mutpair v1)])
                        (right p1))]

           [setleft-exp (exp1 exp2)
                        (let* ([v1 (value-of exp1 env)]
                               [p1 (expval->mutpair v1)]
                               [v2 (value-of exp2 env)])
                          (begin
                            (setleft p1 v2)
                            (num-val 82)))]

           [setright-exp (exp1 exp2)
                         (let* ([v1 (value-of exp1 env)]
                                [p1 (expval->mutpair v1)]
                                [v2 (value-of exp2 env)])
                           (begin
                             (setright p1 v2)
                             (num-val 82)))])))


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

(run
 "let glo = pair(11,22)
  in let f = proc (loc)
               let d1 = setright(loc,left(loc))
               in let d2 = setleft(glo,99)
               in -(left(loc),right(loc))
  in (f glo)")

(run
 "letrec infinite-loop (x) = (infinite-loop -(x,-1))
  in let f = proc (z) 11
  in (f (infinite-loop 0))")
