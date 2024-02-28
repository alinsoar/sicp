#lang racket

(require (submod "e.2.54.rkt" export))

(module+ test

  (car ''abracadabra)

  ''abracadabra

  (quote abracadabra)

  ;; the reader of repl reads 'a as (quote a) during loading/evaluation
  (equal0? ''a (quote (quote a)))

  (equal0? 'a (quote a))

  (equal0? (quote a) (quote a))

  (equal0? ''a (quote a))

  (equal1? ''a (quote (quote a)))

  (equal1? 'a (quote a))

  (equal1? (quote a) (quote a))

  (equal1? ''a (quote a))
  
  (symbol? ''a)

  (pair? ''a)

  (pair? 'a)

  (pair? (quote a))

  (pair? '(quote a))

  (cdr ''a)

  (cdr (quote (quote a))) )
