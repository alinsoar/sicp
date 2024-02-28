#lang racket

(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))
(require (except-in (submod "e.2.77.rkt" export)
                    TAG-TYPE TAG-CONTENTS TAG-ATTACH VALID-DATUM))
(require (submod "e.2.76.rkt" export))

(define (TAG-ATTACH/number-as number-type)
  (lambda (type-tag contents)
    (if (eq? type-tag number-type)
        contents
        (cons type-tag contents))))
(define (TAG-TYPE/number-as number-type)
  (lambda (datum)
    (cond ((pair? datum) (car datum))
          ((number? datum) number-type))))

(define VALID-DATUM (lambda (datum) (or (pair? datum) (number? datum))))
(define TAG-ATTACH (TAG-ATTACH/number-as 'scheme-number))
(define TAG-TYPE (TAG-TYPE/number-as 'scheme-number))
(define TAG-CONTENTS
  (lambda (datum)
    (cond ((pair? datum) (cdr datum))
          ((number? datum) datum))))

(module+ test
  (INVOKE table-operations)
  (INVOKE install-tags-package)
  (INVOKE apply-generic)
  (INVOKE install-basic-packages)
  (magnitude (make-complex-from-real-imag 3 4))
  (make-scheme-number 25)
  (make-rational 1 2)
  (make-integer 100)
  (add 1 2)
  (mul (add 1 2)
       (div 1 2))
  (add (make-scheme-number 10)
       (make-scheme-number 20)))

(module+ export
  (provide VALID-DATUM
           TAG-ATTACH/number-as
           TAG-TYPE/number-as
           TAG-CONTENTS))
