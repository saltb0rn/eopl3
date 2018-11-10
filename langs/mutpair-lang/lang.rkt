#lang racket

(require eopl)

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;; the lexical specification  ;;;;;;;;;;;;;;;;;;
(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "-" "_" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))


;;;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;;;
(define the-grammar
  '([program (expression) a-program]
    [expression (number) const-exp]
    [expression (identifier) var-exp]
    [expression ("zero?" "(" number ")") zero?-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]
    [expression ("letrec"
                 (arbno
                  identifier "(" identifier ")" "=" expression) "in" expression)
                letrec-exp]
    [expression ("begin" expression (arbno ";" expression) "end") begin-exp]
    [expression ("set" identifier "=" expression ) assign-exp]
    [expression ("pair" "(" expression "," expression")") newpair-exp]
    [expression ("left" "(" expression ")") left-exp]
    [expression ("right" "(" expression ")") right-exp]
    [expression ("setleft" "(" expression "," expression ")") setleft-exp]
    [expression ("setright" "(" expression "," expression ")") setright-exp]
    ))

;;;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
