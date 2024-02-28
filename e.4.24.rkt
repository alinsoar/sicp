#lang racket

(require (prefix-in a: racket/base))
(require "sicp.rkt")
(GETMOD 4 1 only the-global-environment)
(GETMOD 4 23)

(module+ test
  (define (code-test evaluator analyze-sequence)
    (define (test-eval e) (evaluator e the-global-environment))
    (o "# DEFINE FUN F") (rp 80 '*) (d)
    (test-eval
      '(define (f x)
         ((lambda (even? odd?)
            (println 'REC x)
            (even? even? odd? x))
          (lambda (ev? od? n)
            (println 'EVEN? n)
            (cond ((= n 0)
                   (print-environment 'env)
                   true)
                  (else (od? ev? od? (- n 1)))))
          (lambda (ev? od? n)
            (println 'ODD?\  n)
            (cond ((= n 0)
                   (print-environment 'env)
                   false)
                  (else (ev? ev? od? (- n 1))))))))
    (o "# MAKE A RECURSIVE CALL of F and COUNT THE TIME") (rp 40 '*) (d)
    (test-eval
     '((lambda (t0)
         (println "even? result:" (f 100))
         (println 'running-time: (- (time) t0)))
       (time))))
  (define (test)
    (define-values/invoke-unit/infer eval/analyze@)
    (code-test eval analyze-sequence))
  (define (test-alyssa)
    (define-values/invoke-unit/infer eval/analyze-alyssa@)
    (code-test eval analyze-sequence))
  "--- FIRST TEST --- BUILTIN EACH ELEMENT OF SEQUENCE"
  (test)
  "--- SECOND TEST --- *ALYSSA* --- BUILTIN EACH SEQUENCE"
  (test-alyssa)
  'done)

