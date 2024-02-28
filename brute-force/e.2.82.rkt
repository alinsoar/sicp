#lang racket

(require (submod "e.2.81.rkt" export))
(require (submod "e.2.76.rkt" export))
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))
(require (except-in (submod "e.2.77.rkt" export)
                    apply-generic@))

(define-unit apply-generic@
  (import table-operations^
          tags^
          coercion-table-operations^)
  (export apply-generic^)
  (define (apply-generic op . args)

    (define (format-list-of-lists l)
      (cond ((null? l) "")
            ((null? (cdr l)) (format "\n\t~a ~a" (car l) (format-list-of-lists (cdr l))))
            (else (format "\n\t~a OR~a" (car l) (format-list-of-lists (cdr l))))))
    
    (define (zip-and-apply functions args)
      (if (null? args)
          '()
          (cons ((car functions) (car args))
                (zip-and-apply (cdr functions) (cdr args)))))  

    (define (zip args operator-procedures)
      (if (null? args)
          '()
          (cons (cons (car operator-procedures) (car args))
                (zip (cdr args) (cdr operator-procedures)))))

    (define (all l)
      (cond ((null? l) true)
            ((false? (car l)) false)
            (else (all (cdr l)))))
    
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))

        (define (make-coercion-procedure-lists type)
          ;; given a `type` we build a list of procedures
          ;; `coercion-procedures` that looks like that:
          ;;
          ;; (T1->TYPE T2->TYPE ... Tn->TYPE)
          ;;
          ;; Each Ti is the ith member of type-tags = (T1 ... Tn)
          ;;
          ;; If, given a `type` we cannot coerce some of the arguments
          ;; to `type`, we return false, because there is no way to
          ;; apply `op` on all arguments coerced to `type`. Otherwise,
          ;; we return the list of procedures `coercion-procedures`,
          ;; that will be used to coerce all each argument to `type`.
          (let ((coercion-procedures 
                 (map (lambda (t)
                        (get-coercion t type))
                      type-tags)))
            (if (all coercion-procedures)
                coercion-procedures
                false)))

        (define (coerce-arguments coercion-procedures-list)
          (zip-and-apply coercion-procedures-list args))

        (define (get-op-coerced-arguments coerced-arguments)
          (get op (map type-tag coerced-arguments)))

        (define (make-valid-applications)
          ;; valid-coercion-the-same-type is a list of lists of
          ;; procedures that convert all arguments to the same type. It
          ;; looks like that:
          ;;
          ;; ( (T1->T1 T2->T1 ... Tn->T1)
          ;;   (T1->T2 T2->T2 ... Tn->T2)
          ;;   (T1->T3 T2->T3 ... Tn->T3)
          ;;   ...
          ;;   (T1->Tn T2->Tn ... Tn->Tn) )
          ;;
          ;; It contains only entries where where we can coerce all
          ;; arguments `args` to the same type.
          (let ((valid-coercions-the-same-type
                 (filter
                  ;; we drop all the coercions to any type Ti, where a
                  ;; coercion function T->Ti is missing.
                  (lambda (x) (not (false? x)))
                  (map (lambda (type) (make-coercion-procedure-lists type))
                       type-tags))))
            ;; now, when we have lists of procedures that can
            ;; correctly coerce all arguments to given types, we start
            ;; coercing the arguments.
            (let ((coerced-arguments-list
                   (map coerce-arguments valid-coercions-the-same-type)))
              ;; after we coerced all arguments to type Ti for all Ti
              ;; where all arguments can be coerced, we apply the
              ;; procedures and coerce the arguments.
              (let ((valid-operators-coerced-arguments
                     (map get-op-coerced-arguments coerced-arguments-list)))
                ;; finally, we check and see whether an operation `op`
                ;; is defined on the coerced arguments, and return a
                ;; list of pairs of the form
                ;;
                ;; ( (operator . args-coerced-to-type-1)
                ;;   (operator . args-coerced-to-type-2) ... )
                ;;
                ;; , 1 pair for each type-i, where the application is possible.
                (filter (lambda (x) (not (false? (car x))))
                        (zip coerced-arguments-list
                             valid-operators-coerced-arguments))))))
        (if proc
            (apply proc (map contents args))
            (let ((valid-operator-applications (make-valid-applications)))
              (if (null? valid-operator-applications)
                  (error (format "there is no operator `~a` ~a -- ~a."
                                 op
                                 "to be applied on the same argument types"
                                 (format-list-of-lists
                                  (map (lambda (t) (map (lambda (t0) t) type-tags))
                                       type-tags))))
                  ;; we select the first entry from the list of possible
                  ;; applications. For example, (ADD NUMBER RATIONAL)
                  ;; will return at this point 2 entries, because there
                  ;; are 2 valid applications (ADD NUMBER NUMBER) and
                  ;; (ADD RATIONAL RATIONAL). We could select any of
                  ;; them.
                  (apply (caar valid-operator-applications)
                         (map contents (cdar valid-operator-applications))))))))))

;;; There are many ways to solve this problem.
;;;
;;; Here is a way.
;;;
;;; Suppose we defined an exponentiation operation like that:
;;; 
;;; |               | scheme-number | rational | complex     |
;;; | scheme-number |               |          |             |
;;; | rational      |               |          |             |
;;; | complex       |               |          | exponential |
;;;
;;; Suppose as well that we can coerce in these directions:
;;; 
;;; |               | scheme-number | rational | complex |
;;; | scheme-number | ok            | ok       | ok      |
;;; | rational      | ok            | ok       | ok      |
;;; | complex       |               |          | ok      |
;;;
;;; Now, because we can coerce from number to complex and from
;;; rational to complex, we are expected to be able to compute
;;; (exponential NUMBER NUMBER) or (exponential NUMBER RATIONAL), etc.
;;; Hoever, our system is programmed in apply-generic to try and
;;; compute only (exponential NUMBER NUMBER) and (exponential RATIONAL
;;; RATIONAL), so it will not find the definition of exponential in
;;; (exponential COMPLEX COMPLEX), even if the system has all the
;;; prerequisites to do this computation.
;;;
;;; the test cases after `apply-generic` proves this very idea.

(module+ test
  (require (rename-in (only-in (prefix-in euler: racket/base )
                               euler:exp)
                      (euler:exp e)))
  (require rackunit)
  ;; installing the tags requires nothing
  (INVOKE install-tags-package)
  ;; reset the coercion-table
  (INVOKE table-operations
    (import)
    (export (rename table-operations^
                    (put-coercion put)
                    (get-coercion get)
                    (show-coercion table-show))))
  ;; reset the global-table
  (INVOKE table-operations)
  ;; install apply-generic
  (INVOKE apply-generic)
  
  (INVOKE-GENERIC exp)
  
  (INVOKE install-basic-packages)
  ;; install coercion table
  (INVOKE extend-scheme-number-package/coercions)
  (INVOKE extend-rational-package/coercions)
  (INVOKE extend-complex-package/coercions)
  ;; install exponential in global-table ONLY for complex numbers
  (INVOKE extend-complex-package/exp)

  (get-tables table-show show-coercion)
  
  (add (make-rational 1 2)
       (make-complex-from-mag-ang .5 0))

  ;; exponential of complex and something works
  (exp (make-complex-from-mag-ang .5 0)
       (make-rational 1 2))

  ;; exponential of something and complex works
  (exp (make-scheme-number 2)
       (make-complex-from-mag-ang .5 0))
  
  (add (make-rational 1 2)
       (make-complex-from-mag-ang .5 0))

  (add (make-complex-from-mag-ang 1 2)
       (make-complex-from-mag-ang .5 0))

  (add (make-rational 1 2)
       (make-scheme-number .5))

  (mul (make-rational 1 2)
       (make-scheme-number .5))

  (mul (make-scheme-number .5)
       (make-complex-from-real-imag 1 2))

  (exp (make-complex-from-real-imag (e 1) 0) ; e^i*pi = -1.
       (make-complex-from-real-imag 0 pi))

  ;; exp of something non-complex does not work
  (check-exn exn:fail?
             (lambda ()
               (exp (make-scheme-number 16)
                    (make-scheme-number .5)))
             'ok)

  (check-exn exn:fail?
             (lambda ()
               (exp (make-rational 1 2)
                    (make-scheme-number .5)))
             'ok))

