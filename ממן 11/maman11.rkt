#lang scheme
(require "utils.scm")

;----------Implementations----------
;Question 1A:
(define (my_append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (my_append (cdr lst1) lst2))))

;Question 1B:
(define (my_append_fr lst1 lst2)
  (foldr cons lst2 lst1))

;Question 2:
(define (filter predicate lst)
  (foldr (lambda (x filtered_lst)
           (if (predicate x)
               (cons x filtered_lst)
                filtered_lst))
            '()
            lst))

;Question 3:
(define (powerset lst)
  (if (null? lst)
      '(())
  (let ((rest (powerset (cdr lst))))
  (my_append (map (lambda (subset) (cons (car lst) subset)) rest) 
              rest))))

;----------Tests----------
;Question 1A Tests:
(equal?? (my_append '(a b c) '(x y z)) '(a b c x y z))
(equal?? (my_append '(1 2) '(5 4)) '(1 2 5 4))


;Question 1B Tests:
(equal?? (my_append_fr '(a b c) '(x y z)) '(a b c x y z))
(equal?? (my_append_fr '(1 2) '(5 4)) '(1 2 5 4))

;Question 2 Tests:
(equal?? (filter symbol? '(a 2 b 4 c)) '(a b c))
(equal?? (filter number? '(a 2 b 4 c)) '(2 4))

;Question 3 Tests:
(equal?? (powerset '(3 2 1)) '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))
(equal?? (powerset '()) '(()))