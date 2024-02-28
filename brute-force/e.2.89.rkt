#lang racket

(require "sicp.rkt")
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))
(require (submod "e.2.87.rkt" export))
(require (submod "e.2.76.rkt" export))
(require (submod "e.2.77.rkt" export))
(require (submod "e.2.81.rkt" export))
(require (submod "e.2.80.rkt" export))
(require (submod "e.2.84.rkt" export))
(require (submod "e.2.88.rkt" export))
(require (except-in (submod "e.2.83.rkt" export)
                    extend-complex-package/raise@))

(require racket/trace)

'(
;;; 2 functions are modified so:
;;;
;;; ADJOIN-TERM
;;;
;;; (cons term term-list) =>
;;;        (append (cons (coeff term) term-list)
;;;                (make-list-of-zeros (- (order term) (length term-list))))
;;;
;;; FIRST-TERM
;;;
;;; (car term-list) => (list (sub1 (length term-list))
;;;                          (car term-list))
;;;
;;; The other definitions are kept here in order to link with the new
;;; local definitions of ADJOIN-TERM and FIRST-TERM.
)

(define-unit install-polynomial-package/dense@
  (import apply-generic^ table-operations^ tags^
          constructors/int/num/rat/complex-polar/complex-rect^
          GENERIC/=zero?^
          GENERIC/add/sub/mul/div^)
  (export phyla-polynomial-package-internals^
          populate-table^)
  (INVOKE install-polynomial-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (let ((term-pow (order term))
          (term-contents (coeff term)))
      (define (make-dense-adjoin k co)
        (if (= 0 k)
            (co term-list)
            (make-dense-adjoin
             (sub1 k)
             (lambda (x)
               (co (cons (make-scheme-number 0)
                         x))))))
      (make-dense-adjoin
       (- term-pow (length term-list))
       (lambda (x) (cons term-contents x)))))
  '(trace adjoin-term)
  (define (first-term term-list)
    (make-term (sub1 (length term-list))
               (car term-list)))
  ;; interface to rest of the system
  (define (populate-table)
    (install-with-ops/add/mul adjoin-term first-term))
  'done)

(module+ test
  (INVOKE install-tags-package)
  (INVOKE table-operations)
  (INVOKE apply-generic/raise)
  
  (INVOKE install-basic-packages)
  
  (INVOKE GENERIC/=zero?)
  (INVOKE GENERIC/negation)
  
  (INVOKE constructor/polynomial)
  
  ((lambda ()
     (INVOKE (export (prefix p: polynomial-package-internals^))
             install-polynomial-package)
     (INVOKE (export (prefix d: phyla-polynomial-package-internals^)
                     (prefix d: populate-table^))
             install-polynomial-package/dense)
     (INVOKE (export (prefix z: populate-table^))
             extend-polynomial-package/=zero?)
     (INVOKE (export (prefix n/s: populate-table^))
             extend-polynomial-package/neg/sub)
     (n/s:populate-table d:first-term d:adjoin-term p:term-list)
     (d:populate-table)
     (p:populate-table/constructor-poly)     
     (z:populate-table d:first-term p:term-list)))
  
  (INVOKE extend-integer-package/=zero?)
  (INVOKE extend-scheme-number-package/=zero?)
  (INVOKE extend-rational-package/=zero?)
  (INVOKE extend-complex-package/=zero?)
  (INVOKE extend-polar-package/=zero?)
  (INVOKE extend-rectangular-package/=zero?)

  (INVOKE extend-integer-package/negation)
  (INVOKE extend-scheme-number-package/negation)
  (INVOKE extend-rational-package/negation)
  (INVOKE extend-complex-rectangular-package/negation)
  (INVOKE extend-complex-polar-package/negation)
  (INVOKE extend-complex-package/negation)

  (INVOKE extend-integer-package/raise)
  (INVOKE extend-rational-package/raise)
  (INVOKE extend-scheme-number-package/raise)
  (INVOKE extend-complex-package/raise)
  (INVOKE extend-polynomial-package/raise)

  (get-tables table-show (lambda (-) nil))

  (neg (make-polynomial 'x (list (make-scheme-number 20)
                                 (make-scheme-number 2))))

  (neg (make-polynomial 'x (list (make-scheme-number 20)
                                 (make-scheme-number 2))))

  (make-polynomial 'x (list (make-scheme-number 20)
                                 (make-scheme-number 2)))
  
  (add
   (make-polynomial 'x (list (make-scheme-number 20)
                             (make-scheme-number 2)))
   
   (make-polynomial 'x (list (make-scheme-number 20)
                             (make-scheme-number 2)
                             )))
  (mul
   (add
    (mul
     (add
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10))))
     (add
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))))
    (mul
     (add
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10))))
     (add
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10))))))
   (add
    (mul
     (add
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10))))
     (add
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))))
    (mul
     (add
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10))))
     (add
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))
      (make-polynomial 'x (list (make-rational 1 2)
                                (make-scheme-number 10)))))))
  
  (mul
   (make-polynomial 'x (list (make-scheme-number 20)
                             (make-scheme-number 2)
                             ))
   (make-polynomial 'x (list (make-scheme-number 3)
                             (make-scheme-number 0)
                             (make-scheme-number 0))))
  (mul
   (make-polynomial 'x (list (make-scheme-number 20)
                             (make-scheme-number 2)
                             ))
   (make-polynomial 'x (list (make-scheme-number 3)
                             (make-scheme-number 0)
                             (make-scheme-number 7))))
  'done)

(module+ export
  (provide install-polynomial-package/dense@))
