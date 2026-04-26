#lang eopl
(require "utils.scm")

(define empty-env
  (lambda ()
    (lambda (search-var)
      'unbound)))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define is-num?
  (lambda (e x)
    (number? (apply-env e x))))
  
 
;########## Tests ##########
;definitions
(define e1 (empty-env))
(define e2 (extend-env 'y "hello" e1))
(define e3 (extend-env 'x 5 e2))

;is-num?
(equal?? (is-num? e3 'x) #t)
(equal?? (is-num? e3 'y) #f) 
(equal?? (is-num? e3 'z) #f)