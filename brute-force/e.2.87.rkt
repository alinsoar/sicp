#lang racket

(require "sicp.rkt")
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))
(require (submod "e.2.76.rkt" export))
(require (submod "e.2.80.rkt" export))
(require (submod "e.2.77.rkt" export))
(require (submod "e.2.84.rkt" export))
(require (submod "e.2.81.rkt" export))
(require (except-in (submod "e.2.83.rkt" export)
                    extend-complex-package/raise@))
(require racket/trace)
(define-signature phyla-polynomial-package-internals^
  (adjoin-term first-term))
(define-signature constructor/polynomial^
  (make-polynomial))
(define-signature arith-polynomial/add/mul^
  (add-poly mul-poly add-terms mul-terms))
(define-signature polynomial-package-internals^
  (tag
   make-poly variable term-list
   same-variable? variable?
   make-term order coeff
   rest-terms the-empty-termlist
   empty-termlist?
   populate-table/constructor-poly
   install-with-ops/add/mul))

(define-unit extend-complex-package/raise@
  (import apply-generic^ table-operations^ tags^
          constructor/polynomial^)
  (export)
  (INVOKE install-complex-package)
  (define raise-complex
    (lambda (x) (make-polynomial '_ (list (list 0 (tag x))))))
  (put 'raise '(complex) raise-complex)
  'done)
(define-unit extend-polynomial-package/raise@
  (import tags^ table-operations^)
  (export)
  (put 'raise '(polynomial) (lambda (x) false))
  'done)

(define-unit extend-polynomial-package/=zero?@
  (import apply-generic^ table-operations^ tags^
          GENERIC/=zero?^
          GENERIC/add/sub/mul/div^)
  (export populate-table^)
  (INVOKE install-polynomial-package)
  (define (=zero-poly? first-term term-list)
    (lambda (p)
     (define (iter t co)
       (if (empty-termlist? t)
           (co true)
           (iter (rest-terms t)
                 (lambda (x)
                   (co (and x (=zero? (coeff (first-term t)))))))))
     (iter (term-list p) (lambda (x) x))))
  (define (populate-table first-term term-list)
    (put '=zero? '(polynomial) (=zero-poly? first-term term-list)))
  'done)

(define-unit arith-polynomial/add/mul@
  (import tags^
          polynomial-package-internals^
          GENERIC/add/sub/mul/div^
          GENERIC/=zero?^)
  (export arith-polynomial/add/mul^)
  (define add-terms
    (lambda (adjoin-term first-term)
      (lambda (L1 L2)
        (let ((add-terms (add-terms adjoin-term first-term)))
          (cond ((empty-termlist? L1) L2)
                ((empty-termlist? L2) L1)
                (else
                 (let ((t1 (first-term L1)) (t2 (first-term L2)))
                   (cond ((> (order t1) (order t2))
                          (adjoin-term
                           t1 (add-terms (rest-terms L1) L2)))
                         ((< (order t1) (order t2))
                          (adjoin-term
                           t2 (add-terms L1 (rest-terms L2))))
                         (else
                          (adjoin-term
                           (make-term (order t1)
                                      (add (coeff t1) (coeff t2)))
                           (add-terms (rest-terms L1)
                                      (rest-terms L2))))))))))))
  (define mul-terms
    (lambda (adjoin-term first-term)
      (lambda (L1 L2)
        (let ((mul-terms (mul-terms adjoin-term first-term))
              (add-terms (add-terms adjoin-term first-term))
              (mul-term-by-all-terms (mul-term-by-all-terms adjoin-term first-term)))
          (if (empty-termlist? L1)
              (the-empty-termlist)
              (add-terms (mul-term-by-all-terms (first-term L1) L2)
                         (mul-terms (rest-terms L1) L2)))))))
  (define mul-term-by-all-terms
    (lambda (adjoin-term first-term)
      (lambda (t1 L)
        (let ((mul-term-by-all-terms (mul-term-by-all-terms adjoin-term first-term)))
          (if (empty-termlist? L)
              (the-empty-termlist)
              (let ((t2 (first-term L)))
                (adjoin-term
                 (make-term (+ (order t1) (order t2))
                            (mul (coeff t1) (coeff t2)))
                 (mul-term-by-all-terms t1 (rest-terms L)))))))))
  (define add-poly
    (lambda (adjoin-term first-term)
      (lambda (p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       ((add-terms adjoin-term first-term)
                        (term-list p1)
                        (term-list p2)))
            (error "Polys not in same var -- ADD-POLY"
                   (list p1 p2))))))
  (define mul-poly
    (lambda (adjoin-term first-term)
      (lambda (p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       ((mul-terms adjoin-term first-term)
                        (term-list p1)
                        (term-list p2)))
            (error "Polys not in same var -- MUL-POLY"
                   (list p1 p2)))))))
(define-unit install-polynomial-package/core@
  (import apply-generic^ table-operations^ tags^
          GENERIC/=zero?^
          GENERIC/add/sub/mul/div^
          arith-polynomial/add/mul^)
  (export polynomial-package-internals^)
  ;; internal procedures
  (define (tag p) (attach-tag 'polynomial p))
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; [procedures same-variable? and variable? from section 2.3.2]
  (define (same-variable? v1 v2)
    (or (eq? v1 '_)
        (eq? v2 '_)
        (and (variable? v1) (variable? v2) (eq? v1 v2))))
  (define (variable? x) (symbol? x))
  ;; representation of terms and term lists
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;; (define first-term & adjoin-term => dependent on implementation.
  (define (rest-terms term-list) (cdr term-list))
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  ;; interface to rest of the system
  (define (install-with-ops/add/mul adjoin-term first-term)
    (put 'add '(polynomial polynomial)
         (lambda (p1 p2) (tag ((add-poly adjoin-term first-term) p1 p2))))
    (put 'mul '(polynomial polynomial) 
         (lambda (p1 p2) (tag ((mul-poly adjoin-term first-term) p1 p2)))))
  (define (populate-table/constructor-poly)
    (put 'make 'polynomial
         (lambda (var terms) (tag (make-poly var terms)))))
  'done)
(define-compound-unit/infer install-polynomial-package@
  (import apply-generic^ table-operations^ tags^
          GENERIC/=zero?^
          GENERIC/add/sub/mul/div^)
  (export polynomial-package-internals^
          arith-polynomial/add/mul^)
  (link install-polynomial-package/core@
        arith-polynomial/add/mul@))

(define-unit install-polynomial-package/sparse@
  (import apply-generic^ table-operations^ tags^
          GENERIC/=zero?^
          GENERIC/add/sub/mul/div^)
  (export phyla-polynomial-package-internals^
          populate-table^)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term term-list) (car term-list))
  ;; interface to rest of the system
  (INVOKE install-polynomial-package)
  (define (populate-table)
    (install-with-ops/add/mul adjoin-term first-term))
  'done)

(define-unit constructor/polynomial@
  (import table-operations^)
  (export constructor/polynomial^)
  (define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms)))

(module+ test
  (INVOKE install-tags-package)
  (INVOKE table-operations)
  (INVOKE apply-generic/raise)

  (INVOKE install-basic-packages)
  
  (INVOKE-GENERIC =zero?)
  
  ((lambda ()
     (INVOKE (export (prefix p: polynomial-package-internals^))
             install-polynomial-package)
     (INVOKE (export (prefix s: phyla-polynomial-package-internals^)
                     (prefix s: populate-table^))
             install-polynomial-package/sparse)
     (INVOKE (export (prefix z: populate-table^))
             extend-polynomial-package/=zero?)
     (s:populate-table)
     (p:populate-table/constructor-poly)
     (z:populate-table s:first-term p:term-list)))

  (INVOKE constructor/polynomial)
  
  (INVOKE extend-scheme-number-package/=zero?)
  (INVOKE extend-rational-package/=zero?)
  (INVOKE extend-complex-package/=zero?)
  (INVOKE extend-polar-package/=zero?)
  (INVOKE extend-rectangular-package/=zero?)
  
  (INVOKE extend-rational-package/raise)
  (INVOKE extend-scheme-number-package/raise)
  (INVOKE extend-complex-package/raise)
  (INVOKE extend-polynomial-package/raise)

  (get-tables table-show (lambda (-) nil))
  
  (define (0? p) (format "~a => ~a" p (=zero? p)))
  
  (0? (make-complex-from-mag-ang 0 1))
  (0? (make-complex-from-real-imag 0 0))
  (0? (raise (make-complex-from-mag-ang 10 10)))
  (0? (raise (make-complex-from-real-imag 1 2)))
  (0? (add (make-complex-from-real-imag 1 2)
           (make-rational 10 3)))
  (0? (add (make-polynomial
            'y `((1 ,(make-complex-from-real-imag -10 2))))
           (make-polynomial
            'y `((1 ,(make-complex-from-real-imag 10 -2))))))
  (0? (add
       (make-polynomial
        'x `((3 ,(make-rational 1 2))
             (0 ,(make-polynomial
                  'y `((1 ,(make-complex-from-real-imag 10 0)))))))
       (make-polynomial
        'x `((1 ,(make-polynomial
                  'y `((3 ,(make-scheme-number 10))
                       (1 ,(make-scheme-number 20)))))
             (0 ,(make-scheme-number 2))))))
  (0? (mul
       (mul
        (add
         (make-polynomial
          'x `((1 ,(make-rational 0 2))))
         (make-polynomial
          'x `((1 ,(make-complex-from-real-imag 1 1)))))
        (add
         (make-polynomial
          'x `((1 ,(make-rational 1 2))))
         (make-polynomial
          'x `((1 ,(make-complex-from-mag-ang 1 (/ pi 2)))))))
       (make-polynomial
        'x `((1 ,(make-polynomial
                  'y `((3 ,(make-scheme-number 2))
                       (1 ,(make-scheme-number 1)))))))))
  (0? (mul
       (mul
        (add
         (make-polynomial
          'x `((1 ,(make-rational 1 2))))
         (make-polynomial
          'x `((1 ,(make-complex-from-real-imag 1 1)))))
        (add
         (make-polynomial
          'x `((1 ,(make-rational 1 2))))
         (make-polynomial
          'x `((1 ,(make-complex-from-mag-ang 1 (/ pi 2)))))))
       (make-polynomial
        'x `((1 ,(make-polynomial
                  'y `((3 ,(make-scheme-number 10))
                       (1 ,(make-scheme-number 20)))))))))
  (0? (mul
       (make-polynomial
        'x `((2 ,(make-complex-from-mag-ang 2 1))))
       (make-polynomial
        'x `((1 ,(make-polynomial
                  'y `((3 ,(make-scheme-number 10))
                       (1 ,(make-scheme-number 20)))))))))
  (0? '(polynomial
        x
        (3 (rational 0 . 2))
        (1 (polynomial
            y
            (3 (scheme-number . 0))
            (1 (scheme-number . 0))))
        (0 (polynomial
            y
            (1 (complex rectangular 0 . 0))
            (0 (complex rectangular 0 . 0))))))
  (0? (mul
       '(complex polar 2 . 1)
       '(polynomial
         y
         (3 (scheme-number . 10))
         (1 (scheme-number . 20)))))
  'done)

(module+ export
  (provide constructor/polynomial^
           constructor/polynomial@
           arith-polynomial/add/mul^
           polynomial-package-internals^
           phyla-polynomial-package-internals^

           extend-polynomial-package/=zero?@
           
           extend-complex-package/raise@
           extend-polynomial-package/raise@

           install-polynomial-package/core@
           
           arith-polynomial/add/mul@
           install-polynomial-package@
           install-polynomial-package/sparse@))

