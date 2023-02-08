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

; The First Commandment
;   Always ask null? as the first question in expressing any function

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
; Q: the first commandment says "Always ask null? as the first question"
;   but flatten _does not_! Why?
; A: the first question that flatten asks is lat? and lat? _does_ ask null? as its
;   first question. So flatten is asking null? when it asks lat?


(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (member? a (cdr lat)))))))

(check-true (member? 'a '(a b c)))
(check-false (member? 'a '(1 2 3 4 5)))

; Chapter 3 - Cons the Magnificent

; rember removes a member from a list

; The first implementation of rember:
(define rember1
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember1 a (cdr lat))))))))

; This works!
(check-equal?
 '(lettuce and tomato)
 (rember1 'bacon '(bacon lettuce and tomato)))

; Notice we are checking this is *not* equal; and it isn't. But it _should_ be!
(check-not-equal?
 '(bacon lettuce tomato)
 (rember1 'and '(bacon lettuce and tomato)))

(check-equal?
 '(tomato)
 (rember1 'and '(bacon lettuce and tomato)))
; That passes; but '(tomato) is not the right answer!

; The Second Commandment
; Use cons to build lists

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat)
                          (rember a (cdr lat)))))))))

; This works!
(check-equal?
 '(lettuce and tomato)
 (rember 'bacon '(bacon lettuce and tomato)))

(check-equal?
 '(bacon lettuce tomato)
 (rember 'and '(bacon lettuce and tomato)))
; Yay, this works, now!
; When we didn't use cons we did remove the first instance of the desired element, but
;   we did not keep the first element... they were lost. So we needed to cons each of them on
;   before recurring.

; rember, but simpler. We can simplfy rember so it doesn't use two cond expressions
(define rember-simpler
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember-simpler a (cdr lat)))))))

; These two should still pass:
(check-equal?
 '(lettuce and tomato)
 (rember-simpler 'bacon '(bacon lettuce and tomato)))

(check-equal?
 '(bacon lettuce tomato)
 (rember-simpler 'and '(bacon lettuce and tomato)))

; rember only removes the _first_ instance of a
(check-equal?
 '(again again and again and again)
 (rember-simpler 'and '(again and again and again and again)))

; Just for fun, let's make a rember that removes all matches
(define rember-all
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (rember-all a (cdr lat)))
      (else (cons (car lat) (rember-all a (cdr lat)))))))

; These checks should still pass for rember-all:
(check-equal?
 '(lettuce and tomato)
 (rember-all 'bacon '(bacon lettuce and tomato)))

(check-equal?
 '(bacon lettuce tomato)
 (rember-all 'and '(bacon lettuce and tomato)))

; rember-all now removes all matches for a
(check-equal?
 '(again again again again)
 (rember-all 'and '(again and again and again and again)))

; firsts takes a list of lists and returns the first element of each in a list
; or takes an empty list and returns that.

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(check-equal?
 (firsts '((1 2 3 4) (2 3 4 5) (3 4 5 6) (4 5 6 7)))
 '(1 2 3 4))

; The Third Commandment
; When building a list, describe the first typical element and cons it onto the
; natural recursion

; insertR should insert a new element to the right of the element specified
; in a lat
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(check-equal?
 (insertR 'topping 'fudge '(ice cream with fudge for dessert))
 '(ice cream with fudge topping for dessert))

(check-equal?
 (insertR 'jalapeno 'and '(tacos tamales and salsa))
 '(tacos tamales and jalapeno salsa))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(check-equal?
 (insertL 'there 'world '(hello world))
 '(hello there world))

(check-equal?
 (insertL 4 5 '(1 2 3 5 6))
 '(1 2 3 4 5 6))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(check-equal?
 (subst 'Sam 'world '(hello world))
 '(hello Sam))

; subst2 looks like:
; (subst2 new o1 o2 lat)
; and replaces _either_ the first instance of o1 or o2 with new
; ie it will only replace o2 if it doesn't find o1

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) o1) (cons new (cdr lat)))
      ((eq? (car lat) o2) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(check-equal?
 (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
 '(vanilla ice cream with chocolate topping))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(check-equal?
  (multirember 'cup '(coffee cup tea cup and hick cup))
  '(coffee tea and hick))

; multiinsertR should insert new to the right of every instance of old
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(check-equal?
  (multiinsertR 'ho 'hey '(hey hey hey))
  '(hey ho hey ho hey ho))

; multiinsertL coming up next
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(check-equal?
 (multiinsertL 'hey 'ho '(ho ho ho))
 '(hey ho hey ho hey ho))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

(check-equal?
 (multisubst 'Goodbye 'Hello '(Hello Hello Hello))
 '(Goodbye Goodbye Goodbye))


