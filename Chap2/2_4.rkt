#lang racket

(define (empty-stack) null)

(define (empty-stack? stack) (null? stack))

(define (push stack val) (cons val stack))

(define (pop stack) (cdr stack))

(define (top stack) (car stack))
