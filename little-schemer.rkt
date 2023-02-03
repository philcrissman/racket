#lang racket


; Chapter 1 - Toys

; if x is not a pair, and not null, it is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; car
; car gives you the first element of a list
; you can't ask for the car of an empty list!
; -> (car '()) ; not allowed

; the car of '(a b) is 'a
(eq? 'a (car '(a b)))

; the cdr 



; Chapter 2 - Do it, do it again, and again, and again...

(define lat?
  (lambda (l)
    (cond
      ((atom? l) #f)
      ((null? l) #f)
      (else (and (atom? (car l)) (lat? (cdr l)))))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (member? a (cdr lat)))))))



; Chapter 3 - Cons the Magnificent