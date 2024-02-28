#lang racket

(require "sicp.rkt")

(define (make-account balance password)
  "keep a list of passwords for each account."
  (let ((password-list nil))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (joint new-password)
      (set! password-list (cons new-password password-list))
      (string-append "joined new password: " (symbol->string new-password)))
    (define (dispatch pass msg)
      (cond ((not (member pass password-list))
             (error "Incorrect password"))
            ((eq? msg 'withdraw)
             withdraw)
            ((eq? msg 'deposit)
             deposit)
            ((eq? msg 'joint)
             joint)
            ((eq? msg 'get)
             balance)
            (else (error "Unknown request") ) ) )
    (set! password-list (list password))
    dispatch) )

(define (make-joint account password new-password)
  "if Peter changes the password after Paul duplicated his account,
   the system should continue working"
  (let ((joint (account password 'joint)))
    (joint new-password)
    account ) )

(module+ test
  (define peter-acc (make-account 100 'open-sesame))
  (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

  "---"
  ((peter-acc 'open-sesame  'withdraw ) 40)
  (peter-acc 'open-sesame 'get)
  ((peter-acc 'open-sesame  'deposit ) 40)
  (peter-acc 'open-sesame 'get)
  ((peter-acc 'open-sesame  'joint ) 'p)
  (peter-acc 'open-sesame 'get)
  (paul-acc 'rosebud 'get)
  ((paul-acc 'p 'withdraw) 100)
  (paul-acc 'rosebud 'get)
  ((paul-acc 'rosebud 'deposit) 110)
  (paul-acc 'p 'get)
  )


