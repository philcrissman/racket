#lang racket

(define person
  (lambda (initial-name)
    (let ((name initial-name)
          (age 0)
          (messages '()))
      (lambda (msg . args)
        (cond
          ((eq? msg 'get-name) name)
          ((eq? msg 'set-name)
           (set! name (car args)))
          ((eq? msg 'get-age) age)
          ((eq? msg 'set-age)
           (set! age (car args)))
          ((eq? msg 'say)
           (set! messages (cons (car args) messages)))
          ((eq? msg 'get-messages) messages)
          (else ((error "what"))))))))

