#lang racket


; Chapter 1 - Toys

; if x is not a pair, and not null, it is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 'atom)
(atom? 'turkey)
(atom? 1492)

(list? '(atom))
(list? '(atom turkey or))
(list? '((atom turkey) or))

; both lists and atoms are S-expressions

(not (atom? '()))

; car
; car gives you the first element of a list
; you can't ask for the car of an empty list!
; -> (car '()) ; not allowed

; the car of '(a b) is 'a
(eq? 'a (car '(a b)))

; the cdr of '(a b) is '(b)
; eq? does not work here! Why?
(eq? '(b) (cdr '(a b)))



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