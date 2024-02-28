#lang racket

(require "sicp.rkt")
(require (submod "e.2.81.rkt" export))
(require (submod "e.2.84.rkt" export))
(require (submod "e.2.89.rkt" export))
(require (except-in (submod "e.2.87.rkt" export)
                    extend-complex-package/raise@))
(require (submod "e.2.80.rkt" export))
(require (submod "e.2.77.rkt" export))
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))
(require (submod "e.2.76.rkt" export))
(require (except-in (submod "e.2.83.rkt" export)
                    extend-complex-package/raise@))

(require racket/trace)

(define-signature constructor/sparse-polynomial^
  (make-sparse-polynomial))
(define-signature constructor/dense-polynomial^
  (make-dense-polynomial))
(define-signature polynomial-package-internals/sparse/dense^
  (make-sparse-poly make-dense-poly
   make-combined-poly variable type-combined-poly terms-combined-poly
   cast cast-from-dense-to-sparse cast-from-sparse-to-dense
   apply-op-on-poly))

(define-unit extend-complex-package/raise@
  (import apply-generic^ table-operations^ tags^
          constructor/dense-polynomial^
          constructor/polynomial^)
  (export)
  (INVOKE install-complex-package)
  (define raise-complex
    (lambda (x) (make-dense-polynomial '_ (list (tag  x)))))
  (put 'raise '(complex) raise-complex)
  'done)

(define-unit install-polynomial-package/sparse/dense@
  (import apply-generic^ table-operations^ tags^
          constructor/polynomial^
          GENERIC/=zero?^
          constructors/int/num/rat/complex-polar/complex-rect^
          GENERIC/add/sub/mul/div^)
  (export polynomial-package-internals/sparse/dense^
          populate-table^)
  (INVOKE (export (prefix poly: polynomial-package-internals^)
                  arith-polynomial/add/mul^)
          install-polynomial-package)
  (INVOKE (export (prefix sparse: phyla-polynomial-package-internals^))
          install-polynomial-package/sparse)
  (INVOKE (export (prefix dense: phyla-polynomial-package-internals^))
          install-polynomial-package/dense)
  (define (make-sparse-poly variable term-list)
    (make-combined-poly variable 'sparse term-list))
  (define (make-dense-poly variable term-list)
    (make-combined-poly variable 'dense term-list))
  (define (make-combined-poly variable tag term-list)
    (make-polynomial variable (attach-tag tag term-list)))
  (define variable poly:variable)
  (define type-combined-poly cadr)
  (define terms-combined-poly cddr)
  (define (cast term-list first-term adjoin-term tag)
    (define (iter term-list co)
      (if (null? term-list)
          (co (poly:the-empty-termlist))
          (let ((first (first-term term-list)))
            (let ((o (poly:order first))
                  (c (poly:coeff first)))
              (iter (poly:rest-terms term-list)
                    (lambda (x)
                      (co (adjoin-term first x))))))))
    '(trace iter)
    (iter term-list (lambda (x) x)))
  (define (cast-from-dense-to-sparse p)
    (cast p dense:first-term sparse:adjoin-term 'sparse))
  (define (cast-from-sparse-to-dense p)
    (cast p sparse:first-term dense:adjoin-term 'dense))
  (define apply-op-on-poly
    (lambda (op)
      (lambda (p1 p2)
        (if (poly:same-variable? (variable p1) (variable p2))
            (let ((var (cond ((eq? (variable p1) '_) (variable p2))
                             ((eq? (variable p2) '_) (variable p1))
                             ((eq? (variable p1) (variable p2)) (variable p1))
                             (else (error "var"))))
                  (type1 (type-combined-poly p1))
                  (type2 (type-combined-poly p2))
                  (tl1 (terms-combined-poly p1))
                  (tl2 (terms-combined-poly p2)))
              (let ((op (get op (list type1 type2))))
                (if op
                    (make-combined-poly var type1 (op tl1 tl2))
                    (error
                     (format
                      "== cannot find operation ~a for the types [~a; ~a] == ~a ~a."
                      op type1 type2 p1 p2)))))
            (error "Polys not in same var --" op "-POLY"
                   (list p1 p2))))))
  (define (populate-table)
    
    (put 'add '(sparse sparse)
         (lambda (tl1 tl2)
           ((add-terms sparse:adjoin-term sparse:first-term)
            tl1 tl2)))
    (put 'add '(dense dense)
         (lambda (tl1 tl2)
           ((add-terms dense:adjoin-term dense:first-term)
            tl1 tl2)))
    (put 'add '(sparse dense)
         (lambda (tl1 tl2)
           ((add-terms sparse:adjoin-term sparse:first-term)
            tl1 (cast-from-dense-to-sparse tl2))))
    (put 'add '(dense sparse)
         (lambda (tl1 tl2)
           ((add-terms dense:adjoin-term dense:first-term)
            tl1 (cast-from-sparse-to-dense tl2))))
    
    (put 'mul '(sparse sparse)
         (lambda (tl1 tl2)
           ((mul-terms sparse:adjoin-term sparse:first-term)
            tl1 tl2)))
    (put 'mul '(dense dense)
         (lambda (tl1 tl2)
           ((mul-terms dense:adjoin-term dense:first-term)
            tl1 tl2)))
    (put 'mul '(sparse dense)
         (lambda (tl1 tl2)
           ((mul-terms sparse:adjoin-term sparse:first-term)
            tl1 (cast-from-dense-to-sparse tl2))))
    (put 'mul '(dense sparse)
         (lambda (tl1 tl2)
           ((mul-terms dense:adjoin-term dense:first-term)
            tl1 (cast-from-sparse-to-dense tl2))))
    
    (put 'add '(polynomial polynomial) (apply-op-on-poly 'add))
    (put 'mul '(polynomial polynomial) (apply-op-on-poly 'mul))
    
    (put 'make 'dense make-dense-poly)
    (put 'make 'sparse make-sparse-poly)
    
    (poly:populate-table/constructor-poly))
  'done)

;; Constructors
(define-unit constructors/sparse-polynomial@
  (import table-operations^)
  (export constructor/sparse-polynomial^)
  (define (make-sparse-polynomial var terms)
    ((get 'make 'sparse) var terms)))
(define-unit constructors/dense-polynomial@
  (import table-operations^)
  (export constructor/dense-polynomial^)
  (define (make-dense-polynomial var terms)
    ((get 'make 'dense) var terms)))

(module+ test
  (INVOKE install-tags-package)
  (INVOKE table-operations)
  (INVOKE apply-generic/raise)
  
  (INVOKE install-basic-packages)
  
  (INVOKE GENERIC/=zero?)
  (INVOKE constructor/polynomial)
  (INVOKE constructors/sparse-polynomial)
  (INVOKE constructors/dense-polynomial)

  ((lambda () (INVOKE install-polynomial-package/sparse/dense)
           (populate-table)))

  (INVOKE extend-integer-package/=zero?)
  (INVOKE extend-scheme-number-package/=zero?)
  (INVOKE extend-rational-package/=zero?)
  (INVOKE extend-complex-package/=zero?)
  (INVOKE extend-polar-package/=zero?)
  (INVOKE extend-rectangular-package/=zero?)
  (INVOKE extend-polynomial-package/=zero?)

  (INVOKE extend-integer-package/raise)
  (INVOKE extend-rational-package/raise)
  (INVOKE extend-scheme-number-package/raise)
  (INVOKE extend-complex-package/raise)
  (INVOKE extend-polynomial-package/raise)

  (get-tables table-show (lambda (-) nil))

  (mul
   (make-dense-polynomial
    'x (list (make-dense-polynomial
              'y (list (make-scheme-number 7)))
             (make-scheme-number 3)))
   (make-dense-polynomial
    'x (list (make-scheme-number 2)
             (make-scheme-number 1))))

  (make-dense-polynomial
   'x (list (make-scheme-number 1)
            (make-complex-from-real-imag 10 20)
            (make-scheme-number 3)))
  (make-sparse-polynomial
   'x `((4 ,(make-rational 1 2))
        (2 ,(make-scheme-number 1))))
  (make-sparse-polynomial
   'x `((1 ,(make-dense-polynomial
             'y `(,(make-scheme-number 1)
                  ,(make-scheme-number 2)
                  ,(make-scheme-number 1))))))

  (raise (make-complex-from-real-imag 10 20))
  (raise (make-complex-from-mag-ang 10 20))
  
  (add
   (make-sparse-polynomial
    'x `((4 ,(make-scheme-number 4))
         (0 ,(make-scheme-number 1))))
   (make-scheme-number 10))
  (add
   (make-scheme-number 10)
   (make-sparse-polynomial
    'x `((4 ,(make-scheme-number 4))
         (0 ,(make-scheme-number 1)))))
  (add
   (make-sparse-polynomial
    'x `((2 ,(make-scheme-number 1))))
   (make-scheme-number 10))
  (add
   (make-dense-polynomial
    'x `(,(make-scheme-number 1)
         ,(make-complex-from-real-imag 10 20)
         ,(make-scheme-number 3)))
   (make-sparse-polynomial
    'x `((3 ,(make-scheme-number 10))
         (2 ,(make-rational 2 3)))))
  (mul
   (make-dense-polynomial
    'x (list (make-sparse-polynomial
              'y `((3 ,(make-scheme-number 3))))
             (make-scheme-number 11)
             (make-scheme-number 13)))
   (make-sparse-polynomial
    'x `((1 ,(make-dense-polynomial
              'y (list (make-scheme-number 1)
                       (make-scheme-number 2)
                       (make-scheme-number 1)))))))

  ;; TEST:    [x^2 [3y^3] + 11x + 13] * [ x [y^2 + 2y + 1] ]
  ;;          = x^3 [3y^5 + 6y^4 + 3y^3]
  ;;          + x^2 [11y^2 + 22y + 11]
  ;;          + x^1 [13y^2 + 26y + 13]
  (mul
   (make-dense-polynomial
    'x `(,(make-sparse-polynomial
          'y `((3 ,(make-scheme-number 3))))
         ,(make-integer 11)
         ,(make-scheme-number 13)))
   (make-sparse-polynomial
    'x `((1 ,(make-dense-polynomial
              'y `(,(make-scheme-number 1)
                   ,(make-integer 2)
                   ,(make-scheme-number 1)))))))
  'done)

(module+ export
  (provide constructor/sparse-polynomial^
           constructor/dense-polynomial^
           constructors/dense-polynomial@
           constructors/sparse-polynomial@
           polynomial-package-internals/sparse/dense^
           install-polynomial-package/sparse/dense@
           extend-complex-package/raise@))
