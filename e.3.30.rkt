#lang racket

(require "sicp.rkt")
(GETMOD 3 28)
(GETMOD 3 22 q:)

(define (make-bus prefix n agenda)
  (define (iter n co)
    (if (zero? n)
        (co '())
        (iter (sub1 n)
              (lambda (x)
                (cons (make-wire
                       (string-append prefix
                                      (number->string n))
                       agenda)
                      (co x))))))
  (iter n (lambda (x) x)))
(define (instrument-bus-wires bus)
  (if (null? bus)
      'done
      (begin (probe (car bus))
             (instrument-bus-wires (cdr bus)))))
(define (set-signal-bus-wires! bus signal)
  (if (null? bus)
      'done
      (begin (set-signal! (car bus) (car signal))
             (set-signal-bus-wires! (cdr bus) (cdr signal)))))
(define (get-signal-bus-wires bus)
  (if (null? bus)
      '()
      (cons (get-signal (car bus))
            (get-signal-bus-wires (cdr bus)))))
(define (print-bus bus)
  (define (iter bus values co)
    (if (null? bus)
        (co ") -- ( ")
        (iter (cdr bus)
              (cdr values)
              (lambda (x)
                (string-append (co (format "~a ~a" (car values) x))
                               (format "~a " ((car bus) 'get-name)))))))
  (string-append
   (iter bus
         (get-signal-bus-wires bus)
         (lambda (x) (string-append "BUS ( " x )))
   ")"))

(define ripple-carry-adder
  (lambda (busA busB busS C agenda)
    (define (iter busA busB busS co)
      (if (null? busA)
          (co (make-wire "last-carry(ZERO)" agenda))
          (let ((a (car busA)) (b (car busB)) (s (car busS)))
            (iter (cdr busA)
                  (cdr busB)
                  (cdr busS)
                  (lambda (carry)
                    (let ((c-out (make-wire
                                  (string-append "carry-out-"
                                                 (a 'get-name)
                                                 (b 'get-name)
                                                 (s 'get-name))
                                  agenda)))
                      (probe c-out)
                      (full-adder a b carry s c-out agenda)
                      (co c-out)
                      'ok))))))
    (iter busA busB busS
          (lambda (carry) (connector carry C agenda)))))

(module+ test
  (define the-agenda (make-agenda q:make-queue
                                  q:empty-queue?
                                  q:insert-queue!
                                  q:delete-queue!
                                  q:front-queue))
  "---"
  (define N 4)
  (define busA    (make-bus "a" N the-agenda))
  (define busB    (make-bus "b" N the-agenda))
  (define bus-SUM (make-bus "s" N the-agenda))
  (define C (make-wire 'carry the-agenda))
  (define (propagate-and-print-status)
    (print-agenda the-agenda)
    (d ">>>")
    (propagate the-agenda)
    (d (print-bus busA))
    (d (print-bus busB))
    (d (print-bus bus-SUM))
    (print-wire C)
    (d "TIME:" (current-time the-agenda))
    )

  "---------- instrumenting wires"
  (instrument-bus-wires busA)
  (instrument-bus-wires busB)
  (instrument-bus-wires bus-SUM)
  (probe C)

  "---------- define ripple-carry-adder abstraction"
  (ripple-carry-adder busA busB bus-SUM C the-agenda)
  (propagate-and-print-status)

  "---------- signal propagation -- 32 time units."
  (set-signal-bus-wires! busA '(0 0 1 1))
  (set-signal-bus-wires! busB '(0 0 1 1))
  (propagate-and-print-status)

  "---------- signal propagation -- 32 time units."
  (set-signal-bus-wires! busA '(1 0 1 1))
  (set-signal-bus-wires! busB '(0 0 0 1))
  (propagate-and-print-status)
  
  "---------- signal propagation -- 24 time units."
  (set-signal-bus-wires! busA '(0 0 0 0))
  (set-signal-bus-wires! busB '(0 0 0 0))
  (propagate-and-print-status)

  "---------- set carry to 1 and sum to 0 -- 68 time units."
  (set-signal-bus-wires! busA '(1 1 1 1))
  (set-signal-bus-wires! busB '(0 0 0 1))
  (propagate-and-print-status))
