#lang eopl
(require "utils.scm")

(define-datatype poly poly?
  (zero)
  (make-poly
    (coefficient number?)
    (exponent number?))
  (add-poly
    (p1 poly?)
    (p2 poly?)))

(define degree
  (lambda (p)
    (cases poly p
      (zero () (eopl:error 'degree "Zero polynom doesn't have a degree" p))
      (make-poly (coefficient exponent) exponent)
      (add-poly (p1 p2)
               (if (> (degree p1) (degree p2))
                   (degree p1)
                   (degree p2))))))

(define coeff
  (lambda (p n)
    (cases poly p
      (zero () 0)
      (make-poly (coefficient exponent)
                 (if (= exponent n)
                     coefficient
                     0))          
      (add-poly (p1 p2)
                (+ (coeff p1 n) (coeff p2 n))))))

(define print-poly
  (lambda (p)
    (cases poly p
      (zero () '())
      (make-poly (coefficient exponent)
                 (list (list coefficient exponent)))
      (add-poly (p1 p2)
                (append (print-poly p1) (print-poly p2))))))

(define Calc-poly
  (lambda (p x)
    (cases poly p
      (zero () 0)
      (make-poly (coefficient exponent) 
                 (* coefficient (expt x exponent)))
      (add-poly (p1 p2)
                (+ (Calc-poly p1 x) (Calc-poly p2 x))))))

(define is-zero?
  (lambda (p)
    (cases poly p
      (zero () #t)
      (make-poly (coefficient exponent) #f)
      (add-poly (p1 p2) (and (is-zero? p1) (is-zero? p2))))))

;########## Tests ##########
;definitions
(define p-zero (zero))                   ; 0
(define p1 (make-poly 3 2))              ; 3x^2
(define p2 (make-poly 5 4))              ; 5x^4
(define p3 (add-poly p1 p2))             ; 3x^2 + 5x^4
(define p4 (add-poly p-zero p-zero))     ; 0 + 0

;degree
(equal?? (degree p1) 2)
(equal?? (degree p2) 4)
(equal?? (degree p3) 4)

;coeff
(equal?? (coeff p-zero 0) 0)
(equal?? (coeff p-zero 1) 0)
(equal?? (coeff p1 2) 3)
(equal?? (coeff p2 4) 5)
(equal?? (coeff p3 4) 5)
(equal?? (coeff p3 2) 3)
(equal?? (coeff p3 500) 0)
(equal?? (coeff p4 0) 0)
(equal?? (coeff p4 1) 0)

;print-poly
(equal?? (print-poly p-zero) '())
(equal?? (print-poly p1) '((3 2)))
(equal?? (print-poly p2) '((5 4)))
(equal?? (print-poly p3) '((3 2) (5 4)))
(equal?? (print-poly p4) '())

;Calc-poly
(equal?? (Calc-poly p-zero 0) 0)
(equal?? (Calc-poly p-zero 123) 0)
(equal?? (Calc-poly p1 2) 12)
(equal?? (Calc-poly p2 1) 5)
(equal?? (Calc-poly p3 2) 92)
(equal?? (Calc-poly p4 25) 0)

;is-zero?
(equal?? (is-zero? p-zero) #t)
(equal?? (is-zero? p1) #f)
(equal?? (is-zero? p2) #f)
(equal?? (is-zero? p3) #f)
(equal?? (is-zero? p4) #t)