#lang racket

(require "sicp.rkt")
{require (only-in racket/base [apply apply/scheme/internal])}

;;; STREAM SUPPORT USEFUL FOR THE QUERY LANGUAGE

(define S:empty '())
(define S:null? (lambda (stream) (null? stream)))
(define S:car (lambda (stream) (car stream)))
(define S:cdr (lambda (stream) (S:force (cdr stream))))

(define active-memo-proc true)
(define set-active-memo-proc! (lambda (v) (set! active-memo-proc v)))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))
(define S:force (lambda (delayed-object)
                (delayed-object)))
;;; DELAY AND CONS are not functions, they ARE SPECIAL FORMS
(define-syntax-rule (S:delay expr)
  ((if active-memo-proc
       memo-proc
       (lambda (x) x))
   (lambda () expr)))
(define-syntax-rule (S:cons a b)
  (cons a (S:delay b)))

(define (S:ref s n)
  (if (= n 0)
      (S:car s)
      (S:ref (S:cdr s) (- n 1))))
(define (S:map proc argstreams)
  (if (S:null? argstreams)
      S:empty
      (S:cons (proc (S:car argstreams))
              (S:map proc (S:cdr argstreams)))))

(define (display-S:row s)
  (S:for-each display-row s))
(define (display-row x)
  (display (~a x #:width 12 #:align 'right))
  (display ","))
(define (show x)
  (display-line x)
  x)
(define (print-row-n s n)
  (if (> n 0)
      (begin (display-row (S:car s))
             (print-row-n (S:cdr s) (- n 1)))
      (newline)))
(define (print-column-n s n)
  (if (> n 0)
      (begin (newline)
	     (display (S:car s))
             (print-column-n (S:cdr s) (- n 1)))
      (newline)))

(define (S:for-each proc s)
  (if (S:null? s)
      'done
      (begin (proc (S:car s))
             (S:for-each proc (S:cdr s)))))
(define (display-line x)
  (newline)
  (display x))
(define (S:filter pred stream)
  (cond ((S:null? stream) S:empty)
        ((pred (S:car stream))
         (S:cons (S:car stream)
                      (S:filter pred
                                (S:cdr stream))))
        (else (S:filter pred (S:cdr stream)))))
(define (S:append s1 s2)
  (if (S:null? s1)
      s2
      (S:cons (S:car s1)
                   (S:append (S:cdr s1) s2))))
(define (S:interleave s1 s2)
  (if (S:null? s1)
      s2
      (S:cons (S:car s1)
              (S:interleave s2 (S:cdr s1)))))
(define (S->list s)
  (if (S:null? s)
      '()
      (cons (S:car s)
            (S->list (S:cdr s)))))

;;;Stream operations

(define (S:append-delayed s1 delayed-s2)
  (if (S:null? s1)
      (S:force delayed-s2)
      (S:cons
       (S:car s1)
       (S:append-delayed (S:cdr s1) delayed-s2))))
(define (S:interleave-delayed s1 delayed-s2)
  (if (S:null? s1)
      (S:force delayed-s2)
      (S:cons
       (S:car s1)
       (S:interleave-delayed (S:force delayed-s2)
                             (S:delay (S:cdr s1))))))
(define (S:flatmap proc s)
  (S:flatten (S:map proc s)))
(define (S:flatten stream)
  (if (S:null? stream)
      S:empty
      (S:interleave-delayed
       (S:car stream)
       (S:delay (S:flatten (S:cdr stream))))))
(define (S:singleton x)
  (S:cons x S:empty))
(define (list->S l)
  (if (null? l)
      S:empty
      (S:cons (first l)
              (list->S (rest l)))))
(define (S:display s)
  (if (S:null? s)
      (newline)
      (begin (displayln
              (string-upcase
               (pretty-format (S:car s)
                              (pretty-print-columns))))
             (S:display (S:cdr s)))))

(provide S:flatmap
         S:append-delayed
         S:empty
         S:interleave-delayed
         S:interleave
         S:null?
         S:singleton
         apply/scheme/internal
         S:append
         S:cons
         S->list
         S:map
         list->S
         S:car
         S:cdr
         S:delay
         S:display
         S:for-each
         S:filter
         )

