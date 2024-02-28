#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass msg)
    (cond ((not (eq? pass password))
           (lambda (_) "Incorrect password"))
          ((eq? msg 'withdraw)
           withdraw)
          ((eq? msg 'deposit)
           deposit)
          (else (lambda (_) "Unknown request")) ) )
  dispatch)

(module+ test
  
  (define acc (make-account 100 'pass))

  ((acc 'pass  'withdraw ) 40)
  
  ((acc 'pass0  'withdraw ) 40)

  ((acc 'pass  'withdr ) 40)

  ((acc 'pass  'withdraw ) 61)
  
  ((acc 'pass  'deposit ) 40))

