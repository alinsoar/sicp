#lang racket

(define the-empty-stream '())
(define stream-null? (lambda (stream) (null? stream)))
(define stream-car (lambda (stream) (car stream)))
(define stream-cdr (lambda (stream) (force (cdr stream))))

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
(define force (lambda (delayed-object)
                (delayed-object)))
;;; DELAY AND CONS are not functions, they ARE SPECIAL FORMS
(define-syntax-rule (delay expr)
  ((if active-memo-proc
       memo-proc
       (lambda (x) x))
   (lambda () expr)))
(define-syntax-rule (cons-stream a b)
  (cons a (delay b)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream (proc (stream-car s))
;;                    (stream-map proc (stream-cdr s)))))
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map
                          (cons proc (map stream-cdr argstreams))))))

(define (display-line x)
  (newline)
  (display x))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-stream-row s)
  (stream-for-each display-row s))
(define (display-row x)
  (display (~a x #:width 12 #:align 'right))
  (display ","))
(define (show x)
  (display-line x)
  x)
(define (print-row-n s n)
  (if (> n 0)
      (begin (display-row (stream-car s))
             (print-row-n (stream-cdr s) (- n 1)))
      (newline)))
(define (print-column-n s n)
  (if (> n 0)
      (begin (newline)
	     (display (stream-car s))
             (print-column-n (stream-cdr s) (- n 1)))
      (newline)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers
  (integers-starting-from 1))
(define ones
  (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (div-streams s1 s2)
  (stream-map (lambda (a b) (/ (+ 0. a) b)) s1 s2))
(define (pair-streams s1 s2)
  (stream-map (lambda (a b) (cons a (list b))) s1 s2))

(module+ test
  (print-row-n (stream-enumerate-interval 0 100) 100)
  (print-column-n (stream-enumerate-interval 100 200) 10)
  (print-row-n (stream-map +
                           (stream-enumerate-interval 100 200)
                           (stream-enumerate-interval 0 100))
               100)
  'done)

(module+ export
  (provide stream-car
           stream-cdr
           stream-ref
           stream-filter
           the-empty-stream
           cons-stream
           print-row-n
           print-column-n
           stream-map
           display-line
           display-row
           display-stream
           display-stream-row
           stream-enumerate-interval
           show
           add-streams
           mul-streams
           div-streams
           scale-stream
           pair-streams
           integers
           ones
           integers-starting-from
           stream-null?
           set-active-memo-proc!
           ;; useful in query/eval
           force
           delay
           ))
