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

; The Fourth Commandment
; Always change at least one argument when recurring
; Always change close to termination. The changin argument must be the one tested in the termination 
; condition; when using cdr, test with null?

; Chapter Four - Numbers Games

(check-pred atom? 14)

(check-eq?
  (add1 67)
  68)

(check-eq?
  (sub1 5)
  4)

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(check-eq?
  (o+ 46 12)
  58)

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(check-eq?
  (o- 14 6)
  8)

(check-eq?
  (o- 45 3)
  42)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(check-eq?
  (addtup '(1 2 3 4 5))
  15)

; The First Commandment (revised)
; When recurring on a list of atoms (lat), ask 2 questions: null? and else
; When recurring on a number, ask 2 questions: zero? and else

(define ox
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (ox n (sub1 m)))))))

(check-eq?
  (ox 6 7)
  42)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(check-equal?
  (tup+ '(1 2 3 4 5) '(5 4 3 2 1))
  '(6 6 6 6 6))

(check-equal?
  (tup+ '(1 2) '(3 4 5))
  '(4 6 5))

(check-equal?
  (tup+ '(1 2 3 4 5) '(4 2))
  '(5 4 3 4 5))

; greater than op
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(check-true
  (o> 24 23))

(check-false
  (o> 3 3))

; less than op
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(check-true
  (o< 2 4))

(check-false
  (o< 4 4))

; o= is the = (equality, not assignment) operator
(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

(check-false
  (o= 3 4))

(check-false
  (o= 4 3))

(check-true
  (o= 25 25))

; oex is expt operator
(define oex
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (ox n (oex n (sub1 m)))))))

(check-eq?
  (oex 2 3)
  8)

; oq is quotient, or integer division (no remainder)
(define oq
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (oq (o- n m) m))))))

(check-eq?
  (oq 4 2)
  2)

(check-eq?
  (oq 100 30)
  3)

; length already exists in racket, so i'll call this latlength
(define latlength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (latlength (cdr lat)))))))


(check-eq?
  (latlength '(a b c d e f g))
  7)

; (pick n lat) should return the nth element of lat
(define pick
  (lambda (n lat)
    (cond
      ((o= n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(check-eq?
  (pick 3 '(macaroni and cheese with bacon))
  'cheese)

(check-eq?
  (pick 7 '(a b c d e f g h i j k l m n o p))
  'g)

; one? is defined later, but the book would like us to 
; use it in rempick... so pulling it up here
(define one?
  (lambda (n)
      (o= 1 n)))

(check-true
  (one? 1))

(check-false
  (one? 0))

(check-false
  (one? 42))

; (rempick n lat) _removes_ the nth element from lat and returns the lat wo that element
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(check-equal?
  (rempick 3 '(hotdogs with hot mustard))
  '(hotdogs with mustard))

; (no-nums lat) takes a lat and returns it with no numbers in it, only non-numeric
; atoms
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(check-equal?
 (no-nums '(a 1 2 b c 3 d 4 e))
 '(a b c d e))

(check-equal?
 (no-nums '(1 2 3 4))
 '())

(check-equal?
 (no-nums '(a b c))
 '(a b c))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(check-equal?
  (all-nums '(a 1 2 b c 3 d 4 e))
  '(1 2 3 4))

(check-equal?
  (all-nums '(1 2 3 4))
  '(1 2 3 4))

(check-equal?
  (all-nums '(a b c))
  '())

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(check-true
  (eqan? 1 1))

(check-true
  (eqan? 'a 'a))

(check-false
  (eqan? 'a 3))

(check-false
  (eqan? 2 4))

(check-false
  (eqan? 'a 'b))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(check-eq?
  (occur 'a '(a b c))
  1)

(check-eq?
  (occur 'a '(b c d))
  0)

(check-eq?
  (occur 'hey '(hey ho lets go hey ho lets go))
  2)


; Chapter 4: *Oh My Gawd*: It's Full of Stars

; rember* should remove members even from nested lists
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(check-equal?
  (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
  '((coffee) ((tea)) (and (hick))))

(check-equal?
  (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
  '(((tomato)) ((bean)) (and ((flying)))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
              (cond
                ((eq? (car l) old) (cons (car l) (cons new (insertR* new old (cdr l)))))
                (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(check-equal?
 (insertR* 'ho 'hey '(((hey) lets) (go) (hey (lets (go)))))
 '(((hey ho) lets) (go) (hey ho (lets (go)))))

(check-equal?
 (insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
 '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood))

; inserts new to the Left of old, even if nested
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (cons (car l) (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(check-equal?
 (insertL* 'ba 'da '(((da) bing) da ((boom))))
 '(((ba da) bing) ba da ((boom))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(check-true
 (member* 'chips '((potato) (chips ((with) fish) (chips)))))

(check-false
 ; there's no i in team!
 (member* 'i '((t) (e) (a) (m))))

(define leftmost
  (lambda (l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (car l))
      (else (or (leftmost (car l)) (leftmost (cdr l)))))))

(check-eq?
 (leftmost '((potato) (chips ((with) fish) (chips))))
 'potato)

(check-eq?
 (leftmost '(((hot) (tuna (and)) cheese)))
 'hot)

; in the book, (leftmost '()) is said to be undefined, so they
; do not recurse over the cdr. I thought it would be interesting to keep looking
; for the leftmost _atom_ even if it's not in the first list-within-a-list,
; that is, even if there is an empty list before more lists that may contain an atom.
; So I wrote it to recurse over the car and the cdr. So this behavior is not exactly what
; is described in the book.
(check-eq?
 (leftmost '(((())) ((( hi! ))) ()))
 'hi!)

; write eqlist? using eqan?
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ; this was confusing to me at first; but the first (and ...) is the question,
      ; and the second (and ...) _is_ the return value; if it's true, eqlist? is true,
      ; and vice versa
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(check-true
 (eqlist? '(hello there) '(hello there)))

(check-true
 (eqlist? '((hello) () ((there) ())) '((hello) () ((there) ()))))


(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (+ 1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))


(check-eq?
 (occur* 'chuck '((how) ((much wood would (a wood (chuck)) ((chuck)) if (a ((wood) chuck) (could (chuck)) wood)))))
 4)

