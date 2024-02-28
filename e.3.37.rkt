#lang racket

(require "sicp.rkt")
(GETMOD 3 33)

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((out (make-connector "oADD")))
    (probe out)
    (adder x y out)
    out))

(define (c- x y)
  (let ((out (make-connector "oSUB")))
    (probe out)
    (adder out y x)
    out))

(define (c* x y)
  (let ((out (make-connector "oMUL")))
    (probe out)
    (multiplier x y out)
    out))

(define (c/ x y)
  (let ((out (make-connector "oDIV")))
    (probe out)
    (multiplier out y x)
    out))

(define (cv v)
  (let ((out (make-connector "oCONST")))
    (probe out)
    (constant v out)
    out))

(module+ test

  (define C (make-connector "Celsius temp"))
  (define F (celsius-fahrenheit-converter C))

  (define (print-status)
    (print-connector C)
    (print-connector F)
    (d "--"))

  (probe C)
  (probe F)
  (d ";;")
  (set-value! C 25 'user)
  (print-status)
  (set-value! F 212 'user)
  (forget-value! C 'user)
  (print-status)
  (set-value! F 212 'user)
  (print-status)
  'done)
