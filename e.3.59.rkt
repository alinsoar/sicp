#lang racket

(require "sicp.rkt")
(GETMOD 3 50)

(define integrate-series
  (lambda (s)
    (mul-streams s
                 (div-streams ones
                              integers))))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(module+ test
  (define (test stream)
    (print-row-n stream 20)
    "---")
  (test ones)
  (test integers)
  (test (div-streams ones integers))
  (test (integrate-series ones))
  (test (integrate-series integers))
  (test exp-series)
  (test (scale-stream ones -1))
  (test cosine-series)
  (test sine-series)
  
  
  'done)

(module+ export
    (provide sine-series
             cosine-series
             exp-series
             integrate-series))
