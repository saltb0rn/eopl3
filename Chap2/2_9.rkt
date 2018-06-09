#lang racket

(define (has-binding? env s)
  (or (and (assoc s env) #t) #f))
