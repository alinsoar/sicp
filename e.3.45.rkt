#lang racket

(require "sicp.rkt")
(GETMOD 3 39)

(define heuristics
  (lambda (m)
    (lambda ()
      (/ (random) 100000))))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (d "WITHDRAW" balance)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (d "DEPOSIT" balance)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (deposit account amount)
 ((account 'deposit) amount))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    (d "EXCHANGE" (account1 'balance) (account2 'balance) difference)
    ;; At this point the BALANCE-SERIALIZER of both accounts is in
    ;; use by the call of EXCHANGE from SERIALIZED-EXCHANGE.
    ((account1 'withdraw) difference)   ;THIS AQUIRE X's mutex again
    (error "the semaphore of X was put in 'wait 2 times so far."
           "This point is never reached.")
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (d "## SERIALIZER EXCHANGE" (account1 'balance) (account2 'balance))
    ((serializer1 (serializer2 exchange)) ; 
     account1
     account2)))

(module+ test
  (require racket/sandbox)
  (require rackunit)
  (define x (make-account-and-serializer 10))
  (define y (make-account-and-serializer 20))
  
  "--- SERIALIZED"

  (infinite-loop (lambda () (serialized-exchange x y))
                 "found DEADLOCK"
                 .3
                 "aquire a mutex 2 times")
  
  'done)
