#lang racket
(require rackunit)

; Chapter 1 - Toys

; if x is not a pair, and not null, it is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(check-pred atom? 'atom)
(check-pred atom? 'turkey)
(check-pred atom? 1492)

(check-pred list? '(atom))
(check-pred list? '(atom turkey or))
(check-pred list? '((atom turkey) or))

; both lists and atoms are S-expressions
(define sexp?
  (lambda (x)
    (or (atom? x) (list? x))))

(check-pred not (atom? '()))
(check-pred sexp? '())
(check-pred sexp? 'hello)
(check-pred sexp? '(a b c))

; car
; car gives you the first element of a list
; you can't ask for the car of an empty list!
; -> (car '()) ; not allowed

; the car of '(a b) is 'a
(check-eq? 'a (car '(a b)))

; The Law of Car:
; car is defined only for non-empty lists
; (car '()) will give us an error

; the cdr of '(a b) is '(b)
; eq? does not work here! Why?
(check-not-eq? '(b) (cdr '(a b)))
; eq? compares 2 non-numeric atoms!
; but these are lists:
(check-pred list? '(b))
(check-pred list? (cdr '(a b)))
; equal? can be used to compare lists
(check-equal? '(b c) (cdr '(a b c)))

; If we think of the list '(a b) like this:
; [a] -> [b] -> []
; (car '(a b)) gives us the contents of the first cell in the list: a
; (cdr '(a b)) give us the rest of the list: [b]->[], or, '(b)
; So (cdr '(b)) should give us the empty list:
(check-equal? '() (cdr '(b)))

; The Law of Cdr
; cdr is defined only for non-empty lists
; The cdr of any non-empty list is always another list

; cons adds atoms to the beginnning of lists
(check-equal?
 '(peanut butter and jelly)
 (cons 'peanut '(butter and jelly)))

; cons adds _any_ S-expression to the front of a list
(define s '((help) this))
(define l '(is very ((hard) to learn)))
(check-equal? '(((help) this) is very ((hard) to learn))
        (cons s l))

; the second argument to cons _must_ be a list
(check-pred list? '())
; So
;    `(cons '(a b c) '())`
; just puts '(a b c) in the front of an empty list, nesting it inside.
(check-equal? '((a b c)) (cons '(a b c) '()))

; The Law of Cons
; cons takes 2 arguments
; The first is any S-expression
; The second is any list
; The result is a list


; null? returns true only for the empty list, '():
(check-pred null? '())
(check-pred not (null? '(1 2 3)))

; The little schemer's Law of Null states that null? is defined only for lists
; but in practice null? is false for anything, except the empty list itself
(check-pred not (null? 'atom))

; The law of Eq?
; eq? takes 2 arguments; each must be a non-numeric atom.
; As we saw earlier, we can use equal? instead to compare lists.
; Also, in practice, eq? sometimes works on numeric atoms as well:
(check-eq? 2 (+ 1 1))


; Chapter 2 - Do it, do it again, and again, and again...

; lat? returns true if our argument is a list of atoms
; if it contains a list, it would return #f
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(check-pred lat? '(a b c))
(check-false (lat? '((a) b c)))

; Aside: how would we write flatten? (takes a (possibly nested)  list and returns a lat)
; Let's try
(define flatten
  (lambda (l)
    (cond
      ; if list is a lat, it's already flat. Return it.
      ; note: null list is a lat, so we don't need to check for it separately!
      ((lat? l) l)
      ; if first element is an empty list, discard it
      ((null? (car l)) (flatten (cdr l)))
      ; if first is an atom, cons it to the flattened cdr
      ((atom? (car l)) (cons (car l) (flatten (cdr l))))
      ; if first element is a lat, cons its car to the recurrence of cons its cdr plus l's cdr
      ((lat? (car l)) (cons (car (car l)) (flatten (cons (cdr (car l)) (cdr l)))))
      ; if we get to else, our first element is not '(), not an atom, not a lat
      ; So it is a nested list. We flatten it first, then flatten the resulting list
      (else (flatten (cons (flatten (car l)) (cdr l)))))))

; Does it work?
(check-equal?
 '(a b c)
 (flatten '(a b c)))

(check-equal?
 '(a b c)
 (flatten '(() a b c)))

(check-equal?
 '(a b c)
 (flatten '((a) b c)))

(check-equal?
 '(a b c)
 (flatten '(a (b) c)))

(check-equal?
 '(a b c)
 (flatten '((a (b)) c)))

(check-equal?
 '(a b c d e f)
 (flatten '(((a) b) c (d (e (f))))))

(check-equal?
 '(a)
 (flatten '(((((((a)))))))))

; It seems to work! Am I missing a test case?
; Time to get a snack. And go back to the book.


(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (member? a (cdr lat)))))))

(check-true (member? 'a '(a b c)))
(check-false (member? 'a '(1 2 3 4 5)))

; Chapter 3 - Cons the Magnificent