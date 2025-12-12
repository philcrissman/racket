#lang racket
(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; chapter 11: Welcome Back to the Show

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? (car lat) a)))))

; should tell if an element is first in a list
(check-true (is-first? 1 '(1 2 3)))
(check-false (is-first? 2 '(1 2 3)))


(define two-in-a-row-1?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (or (is-first? (car lat) (cdr lat))
           (two-in-a-row-1? (cdr lat)))))))

; two-in-a-row-1? should tell if a list has the same 2 atoms next to each other
(check-pred two-in-a-row-1? '(1 2 3 3 4))
(check-pred not (two-in-a-row-1? '(1 2 1 2 3)))


(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (two-in-a-row-1? lat))))))


(define two-in-a-row-2?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (is-first-b? (car lat) (cdr lat))))))

; check that two-in-a-row-2? works
(check-pred two-in-a-row-2? '(1 2 3 3 4))
(check-pred not (two-in-a-row-2? '(1 2 1 2 3)))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? (car lat) preceding)
           (two-in-a-row-b? (car lat) (cdr lat)))))))

; check that two-in-a-row-b? works
(check-true (two-in-a-row-b? 1 '(2 3 3 4)))
(check-false (two-in-a-row-b? 1 '(2 3 4)))

; final version of two-in-a-row?
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))

; test that it works!
(check-pred two-in-a-row? '(1 2 3 3 4))
(check-pred not (two-in-a-row? '(1 2 1 2 3)))

; now a different function: sum-of-prefixes
; sum-of-prefixes should add the sum of all preceding values to the next value
; of a list
; eg (sum-of-prefixes '(1 1 1)) should output `(1 2 3)
;    (sum-of-prefixes '(2 4 6)) should output `(2 6 12)

; here's our first skeleton, which the book does not finish, moving on to
; a solutions similar to two-in-a-row-b?
;
(define sum-of-prefixes-proposed
  (lambda (tup)
    (cond
      ((null? tup) '())
      (else '...
       (sum-of-prefixes-proposed (cdr tup))
       '... ))))

; here's sum-of-prefixes-b
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup) '())
      (else (cons ( + sonssf (car tup))
                  (sum-of-prefixes-b
                   (+ sonssf (car tup))
                   (cdr tup)))))))

; hey!
(check-equal? '(1 2 3) (sum-of-prefixes-b 0 '(1 1 1)))

; now we can write sum-of-prefixes, using sum-of-prefixes-b
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(check-equal? '(2 6 12) (sum-of-prefixes '(2 4 6)))

; now a weird function named `scramble`
; I'm going to copy the description from the book:
; The function _scramble_ takes a non-empty
; tup in which no number is greater than its
; length. Each number in the argument is
; treated as a backward index from its own
; position to a point earlier in the tup. The
; result at each position is found by
; counting backwards from the current
; position according to this index
;
; eg:
; (scramble '(1 1 1 3 4 2 1 1 9 2)) => '(1 1 1 1 1 4 1 1 1 9)
; (scramble '(1 2 3 4 5 6 7 8 9))   => '(1 1 1 1 1 1 1 1 1)

; bring back _pick_ from the little schemer!

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))


; equality
(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

(define one?
  (lambda (n)
    (o= n 1)))

(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
       (cons (pick (car tup)
                   (cons (car tup) rev-pre))
             (scramble-b (cdr tup)
                         (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(check-equal? '(1 1 1 1) (scramble '(1 2 3 4)))

; chapter 12: Take Cover

; a new version of multirember from little schemer
; multirember removes multiple instances of a member of a list
; eg (multirember 'tuna '(shrimp salad tuna salad and tuna))
; returns '(shimp salad salad and)
(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? a (car lat))
                   (mr (cdr lat)))
                  (else
                   (cons (car lat)
                         (mr (cdr lat))))))))
       mr)
     lat)))



(check-equal? '(shrimp salad salad and) (multirember 'tuna '(shrimp salad tuna salad and tuna)))

; this isn't anything, just making sure I understand
; how letrec works
(define foo
  (lambda (lat)
    ((letrec
         ((add-3 (lambda (lat)
                      (cond
                        ((null? lat) '())
                        (else
                         (cons (+ 3 (car lat)) (add-3 (cdr lat))))))))
       add-3)
     lat)))


(define multirember-2
  (lambda (a lat)
    (letrec
        ((mr (lambda (lat)
               (cond
                 ((null? lat) '())
                 ((eq? a (car lat))
                  (mr (cdr lat)))
                 (else
                  (cons (car lat)
                        (mr (cdr lat))))))))
      (mr lat))))

(check-equal?
 (multirember-2 'pie '(apple pie custard linzer pie torte))
 '(apple custard linzer torte))

; multirember-f
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)
         ((multirember-f test?) a
                                (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a
                                           (cdr lat))))))))

(check-equal?
 ((multirember-f eq?) 'a '(a b c a d))
 '(b c d))

; member, but with letrec
(define member-a?
  (lambda (a lat)
    (letrec
        ((yes? (lambda (l)
                 (cond
                   ((null? l) #f)
                   ((eq? (car l) a) #t)
                   (else (yes? (cdr l)))))))
      (yes? lat))))

(check-false (member-a? 'q '(a b c)))
(check-true (member-a? 'a '(a b c)))

; union of sets
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))

(check-equal?
 (union '(crosby stills and young) '(stills nash and young))
 '(crosby stills nash and young))

; union, but with letrec, and a local definition of member?
(define union-b
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
                ((null? set) set2)
                ((M? (car set) set2)
                 (U (cdr set)))
                (else (cons (car set)
                            (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond
                            ((null? lat) #f)
                            ((eq? (car lat) a) #t)
                            (else (N? (cdr lat)))))))
               (N? lat)))))
         (U set1))))

(check-equal?
 (union-b '(larry and curly) '(moe and curly))
 '(larry moe and curly))

; two-in-a-row, again
(define two-in-a-row-c?
  (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else (or (eq? (car lat) a)
                        (W (car lat)
                           (cdr lat))))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

(check-true (two-in-a-row-c? '(a b b c d)))
(check-false (two-in-a-row-c? '(a b c d)))


; hey!
(check-equal? '(1 2 3) (sum-of-prefixes-b 0 '(1 1 1)))

; now we can write sum-of-prefixes with letrec:
(define sum-of-prefixes-2
  (lambda (tup)
    (letrec
        ((S (lambda (sss tup)
              (cond
                ((null? tup) '())
                (else (cons (+ sss (car tup))
                            (S (+ sss (car tup))
                               (cdr tup))))))))
      (S 0 tup))))

(check-equal?
 (sum-of-prefixes-2 '(1 1 1 1 1))
 '(1 2 3 4 5))


; ... and scramble can be re-rwritten this way as well
(define scramble-b-2
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
       (cons (pick (car tup)
                   (cons (car tup) rev-pre))
             (scramble-b-2 (cdr tup)
                         (cons (car tup) rev-pre)))))))

(define scramble-2
  (lambda (tup)
    (letrec
        ((S (lambda (tup rp)
             (cond
               ((null? tup) '())
               (else
                (cons (pick (car tup)
                            (cons (car tup) rp))
                      (S (cdr tup)
                         (cons (car tup) rp))))))))
      (S tup 0))))

(check-equal? '(1 1 1 1) (scramble-2 '(1 2 3 4)))

; ch 13: Hop, Skip, and Jump

(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) '())
                ((member? (car set) set2)
                 (cons (car set)
                       (I (cdr set))))
                (else (I (cdr set)))))))
      (I set1))))

(check-equal?
 (intersect '(simon and garfunkel) '(hall and oates))
 '(and))

(check-equal?
 (intersect '(bread and butter)
            (intersect '(toast and jam) '(macaroni and cheese)))
 '(and))

; intersect all takes a list of sets and gives the intersection
; of all of them
(define intersectall-0
  (lambda (lset)
    (cond
      ((null? lset) '())
      ((null? (cdr lset)) (car lset))
      (else (intersect (car lset)
                       (intersectall-0 (cdr lset)))))))

(define intersectall
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
                ((null? (cdr lset))
                 (car lset))
                (else (intersect (car lset)
                                 (A (cdr lset))))))))
      (cond
        ((null? lset) '())
        (else (A lset))))))

;(check-equal?
; (intersectall
;  '(
;    '(bread and butter)
;    '(toast and jam)
;    '(macaroni and cheese)
;    ))
; '(and))

(check-equal?
 (intersectall
  '())
 '())

(check-equal?
 (intersectall
  '((1 2 3)))
 '(1 2 3))

(check-equal?
 (intersectall
  '(
    (3 mangoes and)
    (3 kiwis and)
    (3 hamburgers)))
 '(3))

(define intersectall-2
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
           (letrec
               ((A (lambda (lset)
                     (cond
                       ((null? (car lset))
                        (hop '()))
                       ((null? (cdr lset))
                        (car lset))
                       (else
                        (intersect (car lset)
                                   (A (cdr lset))))))))
             (cond
               ((null? lset) '())
               (else (A lset))))))))

(check-equal?
 (intersectall-2
  '((a b c) (b c d) (c d e)))
 '(c))

(define intersectall-3
  (lambda (lset)
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset))
                   (hop '()))
                   ((null? (cdr lset))
                    (car lset))
                   (else
                    (intersect (car lset)
                               (A (cdr lset))))))))
        (cond
          ((null? lset) '())
          (else (A lset)))))))

(check-equal?
 (intersectall-3
  '((a b c) (b c d) (c d e)))
 '(c))

(check-equal?
 (intersectall-3
  '(() '(a b c d e f g h i) '(d e a f)))
 '())

(check-equal?
 (intersectall-3
  '((bacon and eggs) (this or that) (bacon or sausage)))
 '())

; So, these iterations of intersectall are intended to short-circuit
; the function when we realize there is no more work to do.
; Specifically, if we every wind up with an empty set, or if an empty set is
; one of the original values, we _could_ just stop checking and return '().
; The original version will keep looking for intersections. The above version,
; -3, will stop if there is an empty set in the lset.
; In the last test above, '(bacon and eggs) and '(this or that) have no atoms
; in common; they would return the empty set. So we _should_ just stop checking all
; subsequent sets and return '(). But intersectall-3 will keep going.
; Here's a version that brings `intersect` into the function as I

(define intersectall-4
  (lambda (lset)
    (call-with-current-continuation (lambda (hop)
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset))
                   (hop '()))
                  ((null? (cdr lset))
                   (car lset))
                  (else (I (car lset)
                           (A (cdr lset)))))))
           (I (lambda (s1 s2)
                (letrec
                    ((J (lambda (s)
                          (cond
                            ((null? s) '())
                            ((member? (car s) s2)
                             (cons (car s) (J (cdr s))))
                            (else (J (cdr s)))))))
                  (cond
                    ((null? s2) (hop '()))
                    (else (J s1)))))))
        (cond
          ((null? lset) '())
          (else (A lset))))))))

(check-equal?
 (intersectall-4
  '((a b c) (b c d) (c d e)))
 '(c))

(check-equal?
 (intersectall-4
  '((a b c) (d e f) (b e)))
 '())

; rember with letrec!
(define rember-letrec
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? (car lat) a)(cdr lat))
                (else
                 (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(check-equal?
 (rember-letrec 'coffee '(coffee cream and coffee cake))
 '(cream and coffee cake))

; rember-beyond-first removes the specified atom and all following atoms
(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? (car lat) a) '())
                (else
                 (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(check-equal?
 (rember-beyond-first 'that '(this is a list of things that we will do tomorrow))
 '(this is a list of things))

; rember-upto-last is a function that looks for an atom, and the last time it
; finds it, it just returns the rest of the list. If it never finds it, it returns
; the whole list.
(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
      (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? (car lat) a)
                   (skip (R (cdr lat))))
                  (else
                   (cons (car lat) (R (cdr lat))))))))
        (R lat)))))

; some tests
(check-equal?
 (rember-upto-last 'cookies '(cookies cream and coffee cake and ice cream and cookies crackers and milk))
 '(crackers and milk))

(check-equal?
 (rember-upto-last 'frogs '(chicken and dumplings))
 '(chicken and dumplings))

(check-equal?
 (rember-upto-last 'cream '(cookies cream and ice cream))
 '())

; chapter 14: Let There Be Names

; we are asked to remember the function `leftmost`
; It should extract the leftmost atom from a list of S-expressions

(define leftmost-a
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
       (else (leftmost-a (car l))))))

(check-equal?
 (leftmost-a '(((a) b) (c d)))
 'a)

; leftmost as defined will fail with a list like:
; (() (a b))
; We need to keep looking in the cdr if the car has no atoms

(define leftmost-b
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (cond
              ((atom? (leftmost-b (car l)))
               (leftmost-b (car l)))
              (else (leftmost-b (cdr l))))))))

; now it should work! let's try
(check-equal?
 (leftmost-b '(() (a b)))
 'a)

; in leftmost-b we are repeating the expression (leftmost-b (car l)) ...
; We can use let to only say this once
(define leftmost-c
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
       (let ((a (leftmost-c (car l))))
         (cond
           ((atom? a) a)
           (else (leftmost-c (cdr l)))))))))

; should still work, let's write a couple tests
(check-equal?
 (leftmost-c '((((a) b)) (c d)))
 'a)

(check-equal?
 (leftmost-c '((() a) (b d)))
 'a)