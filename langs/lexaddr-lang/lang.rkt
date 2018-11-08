#lang racket

(require eopl)

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;; the lexical specification ;;;;;;;;;;;;;;;;;;
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

    [expression ("-" "(" expression "," expression ")") diff-exp]

    [expression ("let" identifier "=" expression "in" expression) let-exp]

    [expression ("if" expression "then" expression "else" expression) if-exp]

    [expression ("zero?" "(" expression ")") zero?-exp]

    ;; [expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression)]

    [expression ("proc" "(" identifier ")" expression) proc-exp]

    [expression ("(" expression expression ")") call-exp]

    [expression ("%nameless-var" number) nameless-var-exp]

    [expression ("%let" expression "in" expression) nameless-let-exp]

    [expression ("%lexproc" expression) nameless-proc-exp]

    ;; [expression ("%letrec" expression expression) nameless-letrec-var-exp]
    ))

;;;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
