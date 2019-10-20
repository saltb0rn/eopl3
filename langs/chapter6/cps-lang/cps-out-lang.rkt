#lang racket

(require eopl)

(provide (all-defined-out))

(define cps-out-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define cps-out-grammar
  '([cps-out-program (tfexp) cps-a-program]
    [simple-expression (number) cps-const-exp]
    [simple-expression (identifier) cps-var-exp]
    [simple-expression
     ("-" "(" simple-expression "," simple-expression ")")
     cps-diff-exp]
    [simple-expression
     ("zero?" "(" simple-expression ")")
     cps-zero?-exp]
    [simple-expression
     ("proc" "(" (arbno identifier) ")" tfexp)
     cps-proc-exp]
    [tfexp (simple-expression) simple-exp->exp]
    [tfexp ("let" identifier "=" simple-expression "in" tfexp)
           cps-let-exp]
    [tfexp ("letrec" (arbno identifier "(" (arbno identifier) ")"
                            "=" tfexp) "in" tfexp)
           cps-letrec-exp]
    [tfexp ("if" simple-expression "then" tfexp "else" tfexp) cps-if-exp]
    [tfexp ("(" simple-expression (arbno simple-expression) ")")
           cps-call-exp]))

(sllgen:make-define-datatypes cps-out-lexical-spec cps-out-grammar)

(define cps-show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes cps-out-lexical-spec cps-out-grammar)))

(define cps-out-scan&parse
  (sllgen:make-string-parser cps-out-lexical-spec cps-out-grammar))

(define cps-out-just-scan
  (sllgen:make-string-scanner cps-out-lexical-spec cps-out-grammar))
