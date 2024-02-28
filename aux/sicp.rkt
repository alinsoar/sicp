#lang racket

(provide nil
         GETMOD
         d
         o
         rp
         pv
         f
         join/s
         infinite-loop)

(require racket/sandbox)
(require rackunit)

(define nil '())

(define (o . args) (void (map display args)))

(define (d . args)
  "generic display"
  (define (iter a co)
    (if (null? a)
        (co (lambda () (newline)))
        (iter (cdr a)
              (lambda (x)
                (co (lambda ()
                      (display " ")
                      (display (car a))
                      (x)))))))
  (void (iter args (lambda (x) (x)))))

(define (pv separator . args)
  "print variables"
  (define (iter a co)
    (if (null? a)
        (co (lambda () (newline)))
        (iter (cdr a)
              (lambda (x)
                (co (lambda ()
                      (display (format " ~a "  separator))
                      (display (car a))
                      (x)))))))
  (void (iter args (lambda (x) (x)))))

(define (rp N . patterns)
  "repeat patterns N times"
  (if (zero? N)
      (void)
      (begin (map display patterns)
             (apply rp (cons (sub1 N) patterns)))))

(define (f p . a)
  (d (apply format (cons p a))))

(define (join/s sep tab l)
  (foldr string-append
         ""
         (map (lambda (x)
                (~a x
                    #:pad-string sep
                    #:width tab
                    #:align 'left))
              l )))

(define infinite-loop
  (lambda (TASK MSG TIME TEST-NAME)
    (test-case TEST-NAME
               (check-exn
                exn:fail?
                (lambda ()
                  (call-with-limits TIME 1 TASK)
                  'no-error-caught!))
               MSG)))

(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require (for-syntax racket/format))
(define-for-syntax filename
  (lambda (chapter n)
    (string-append "e." (number->string chapter)
                   "." (~a (number->string n)
                           #:width 2
                           #:pad-string "0"
                           #:align 'right)
                   ".rkt")))
(define-syntax (GETMOD syn)
  (datum->syntax
   syn
   (match (map syntax->datum (syntax->list syn))
     ((list _ chapter n data ...)
      (let ((FILENAME (filename chapter n)))
        (let ((EXPORT (list 'submod FILENAME 'export)))
          (cons
           'require
           (list
            (match data
              ;; ALL MODULE
              ('()
               EXPORT)
              ;; USE A PREFIX
              ((list prefix)
               `(prefix-in ,prefix ,EXPORT))
              ;; ONLY SOME SYMBOLS
              ((list 'only symbols ...)
               (cons 'only-in
                     (cons EXPORT symbols)))
              ;; EXCEPT SOME SYMBOLS
              ((list 'without symbols ...)
               (cons 'except-in
                     (cons EXPORT symbols))))))))))))
