#lang eopl

(define identifier? symbol?)

(define-datatype LcExp LcExp?
  (identifier-exp (identifier identifier?))
  (lambda-exp (identifier identifier?)
	      (lcexp LcExp?))
  (app-exp (rator LcExp?)
	   (rand LcExp?)))
