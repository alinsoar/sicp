#lang racket

;; |-----------+---------+---------------+------+---------------+----------+---------------+---------+---------------+---------+--------+-------|
;; | OP \ TYPE | integer | rational      |      | scheme-number |          | complex       |         |               |         | poly   |       |
;; |-----------+---------+---------------+------+---------------+----------+---------------+---------+---------------+---------+--------+-------|
;; |           |         | scheme-number | poly | tagged        | hardware | angle         |         |               |         |        |       |
;; |           |         |               |      |               |          | magnitude     |         |               |         |        |       |
;; |           |         |               |      |               |          | imag-part     |         |               |         |        |       |
;; |           |         |               |      |               |          | real-part     |         |               |         |        |       |
;; |-----------+---------+---------------+------+---------------+----------+---------------+---------+---------------+---------+--------+-------|
;; |           |         |               |      |               |          | rectangular   |         | polar         |         | sparse | dense |
;; |-----------+---------+---------------+------+---------------+----------+---------------+---------+---------------+---------+--------+-------|
;; |           |         |               |      |               |          | scheme-number | GEN-NUM | scheme-number | GEN-NUM |        |       |
;; |-----------+---------+---------------+------+---------------+----------+---------------+---------+---------------+---------+--------+-------|
;; | add       |         |               |      |               |          |               |         |               |         |        |       |
;; | sub       |         |               |      |               |          |               |         |               |         |        |       |
;; | neg       |         |               |      |               |          |               |         |               |         |        |       |
;; | mul       |         |               |      |               |          |               |         |               |         |        |       |
;; | div       |         |               |      |               |          |               |         |               |         |        |       |
;; | zero?     |         |               |      |               |          |               |         |               |         |        |       |
;; | equ?      |         |               |      |               |          |               |         |               |         |        |       |
;; | exp       |         |               |      |               |          |               |         |               |         |        |       |
;; | raise     |         |               |      |               |          |               |         |               |         |        |       |
;; | drop      |         |               |      |               |          |               |         |               |         |        |       |
;; | project   |         |               |      |               |          |               |         |               |         |        |       |
;; | sine      |         |               |      |               |          |               |         |               |         |        |       |
;; | cosine    |         |               |      |               |          |               |         |               |         |        |       |
;; | atan      |         |               |      |               |          |               |         |               |         |        |       |
;; | square    |         |               |      |               |          |               |         |               |         |        |       |
;; | sqrt      |         |               |      |               |          |               |         |               |         |        |       |
;; |-----------+---------+---------------+------+---------------+----------+---------------+---------+---------------+---------+--------+-------|


(require "sicp.rkt")
(require (submod "e.2.73.rkt" export))
(require (except-in (submod "e.2.77.rkt" export)
                    TAG-TYPE TAG-CONTENTS TAG-ATTACH VALID-DATUM))
(require (submod "e.2.76.rkt" export))
(require (submod "e.2.59.rkt" export))
(require (submod "e.1.44.rkt" export))
(require (submod "e.1.33.rkt" export))
(require (submod "e.2.33.rkt" export))
(require (submod "e.2.78.rkt" export))
(require (rename-in (only-in (submod "e.2.54.rkt" export)
                             equal2?)
                    (equal2? equal?)))
(require (submod "e.2.84.rkt" export))
(require (submod "e.2.80.rkt" export))
(require (submod "e.2.81.rkt" export))
(require (except-in (submod "e.2.87.rkt" export)
                    extend-complex-package/raise@))
(require (except-in (submod "e.2.83.rkt" export)
                    extend-complex-package/raise@))

(define TAG-ATTACH (TAG-ATTACH/number-as 'integer))
(define TAG-TYPE (TAG-TYPE/number-as 'integer))

;;; In this problem, we change the encoding of a polynomial so:
;;;
;;; POLY -> (POLYNOMIAL '(VARS) (TERM-LIST))
;;;
;;; VARS -> V1 V2 ... Vn
;;;
;;; TERM-LIST -> TERM1 TERM2 ... TERMn
;;;
;;; TERMi -> ( (ORDER-LISTi) COEFFi )
;;;
;;; ORDER-LISTi -> ORDER1 ... ORDERn
;;;
;;; ORDERk -> the order of the variable Vk in TERMk
;;;
;;; It is easy to prioritize a variable V and to pass from this
;;; encoding to an encoding that has univeriate polunomials and vice
;;; versa.

(define-unit extend-complex-package/raise@
  (import apply-generic^ table-operations^ tags^
          constructor/polynomial^)
  (export)
  (INVOKE install-complex-package)
  (define raise-complex
    (lambda (x) (make-polynomial '(_) `(((0) ,(tag x))))))
  (put 'raise '(complex) raise-complex)
  'done)

(define-unit install-multivariate-polynomial-package@
  (import apply-generic^ table-operations^ tags^
          GENERIC/=zero?^
          constructor/polynomial^
          GENERIC/add/sub/mul/div^)
  (export populate-table^)
  
  (INVOKE (export (prefix p: polynomial-package-internals^)
                  (prefix p: arith-polynomial/add/mul^))
          install-polynomial-package)
  (INVOKE (export (prefix p: phyla-polynomial-package-internals^))
          install-polynomial-package/sparse)
  
  (define (get-assoc-order var var-list order-list)
    (define (iter v o co)
      (cond ((null? v) (co 0))
            ((null? o) (co 0))
            ((p:same-variable? var (car v)) (co (car o)))
            (else (iter (cdr v)
                        (cdr o)
                        (lambda (x) (co x))))))
    (iter var-list order-list (lambda (x) x)))
  (define (same-order? var1 var2 order1 order2)
    (define (iter v)
      (cond ((null? v) true)
            ((null? o) false)
            (else (and (= (get-assoc-order (car v) var1 order1)
                          (get-assoc-order (car v) var2 order2))
                       (iter (cdr v))))))
    (and (iter var1)
         (iter var2)))
  (define (match? var1 order1 var2 term-list2)
    (define (iter term-list co)
      (cond ((p:empty-termlist? term-list)
             (co false))
            ((same-order? var1 var2 order1
                          (p:order (p:first-term term-list)))
             (co (p:first-term term-list)))
            (else (iter (p:rest-terms term-list)
                        (lambda (x) (co x))))))
    (iter term-list2 (lambda (x) x)))
  (define (remove-term term term-list)
    (define (iter l co)
      (if (equal? (p:order term) (p:order (p:first-term l)))
          (co (cdr l))
          (iter (cdr l)
                (lambda (x)
                  (co (cons (p:first-term l) x))))))
    (iter term-list (lambda (x) x)))
  (define (project-term u-vars vars term)
    (define (iter v co)
      (if (null? v)
          (co '())
          (iter (cdr v)
                (lambda (x)
                  (co (cons (get-assoc-order (car v) vars (p:order term))
                            x))))))
    (p:make-term (iter u-vars (lambda (x) x))
                 (p:coeff term)))
  (define (make-u-vars p1 p2)
    (filtered-accumulate (lambda (x) (not (eq? (car x) '_)))
                         eq?
                         (lambda (x y) (cons y x))
                         '()
                         car
                         (union-set (p:variable p1) (p:variable p2))
                         cdr
                         '()))

  (define (add-terms var1 var2)
    (lambda (u-vars term-list term-list2)
      (let ((add-terms (add-terms var1 var2)))
        (cond ((and (p:empty-termlist? term-list)
                    (p:empty-termlist? term-list2))
               (p:the-empty-termlist))
              ((p:empty-termlist? term-list2)
               (cons (project-term u-vars
                                   var1
                                   (p:first-term term-list))
                     (add-terms u-vars (p:rest-terms term-list) term-list2)))
              ((p:empty-termlist? term-list)
               (cons (project-term u-vars
                                   var2
                                   (p:first-term term-list2))
                     (add-terms u-vars term-list (p:rest-terms term-list2))))
              (else (let ((m (match? var1
                                     (p:order (p:first-term term-list))
                                     var2
                                     term-list2)))
                      (if m
                          (cons (project-term u-vars
                                              var1
                                              (p:make-term
                                               (p:order (p:first-term term-list))
                                               (add (p:coeff (p:first-term term-list))
                                                    (p:coeff m))))
                                (add-terms u-vars
                                           (p:rest-terms term-list)
                                           (remove-term m term-list2)))
                          (cons (project-term u-vars
                                              var1
                                              (p:first-term term-list))
                                (add-terms u-vars
                                           (p:rest-terms term-list)
                                           term-list2)))))))))
  (define add-poly
    (lambda (p1 p2)
      (let ((u-vars (make-u-vars p1 p2)))
        (make-polynomial u-vars
                         ((add-terms (p:variable p1) (p:variable p2))
                          u-vars
                          (p:term-list p1)
                          (p:term-list p2))))))

  (define mul-accumulate
    (lambda (u-vars term-list)
      (define (iter term-list co)
        (if (p:empty-termlist? term-list)
            (co (p:the-empty-termlist))
            (iter (p:rest-terms term-list)
                  (lambda (x)
                    (co ((add-terms u-vars u-vars)
                         u-vars
                         (p:adjoin-term (p:first-term term-list)
                                        (p:the-empty-termlist))
                         x))))))
      (iter term-list (lambda (x) x))))
  (define mul-term-by-all-terms
    (lambda (t var1 var2 u-vars term-list2)
      (define (add-orders v o1 o2 co)
        (if (null? v)
            (co '())
            (add-orders
             (cdr v)
             o1 o2
             (lambda (x)
               (co (cons (+ (get-assoc-order (car v) var1 o1)
                            (get-assoc-order (car v) var2 o2))
                         x))))))
      (define (iter term-list2 co)
        (if (p:empty-termlist? term-list2)
            (co (p:the-empty-termlist))
            (iter (p:rest-terms term-list2)
                  (lambda (x)
                    (co (cons (p:make-term (add-orders u-vars
                                                       (p:order t)
                                                       (p:order (p:first-term term-list2))
                                                       (lambda (x) x))
                                           (mul (p:coeff t)
                                                (p:coeff (p:first-term term-list2))))
                              x))))))
      (iter term-list2 (lambda (x) x))))
  (define (mul-terms p1 p2)
    (lambda (u-vars term-list1 term-list2)
      (let ((mul-terms (mul-terms p1 p2)))
        (if (p:empty-termlist? term-list1)
            (p:the-empty-termlist)
            (append
             (mul-term-by-all-terms (p:first-term term-list1)
                                    (p:variable p1)
                                    (p:variable p2)
                                    u-vars
                                    term-list2)
             (mul-terms u-vars
                        (p:rest-terms term-list1)
                        term-list2))))))
  (define mul-poly
    (lambda (p1 p2)
      (let ((u-vars (make-u-vars p1 p2)))
        (let ((m ((mul-terms p1 p2)
                  u-vars
                  (p:term-list p1)
                  (p:term-list p2))))
          (make-polynomial u-vars (mul-accumulate u-vars m))))
      ))
  
  (define (populate-table)
    (put 'add '(polynomial polynomial) add-poly)
    (put 'mul '(polynomial polynomial) mul-poly)))

(module+ test
  (begin 
    (INVOKE table-operations)
    (INVOKE install-tags-package)
    (INVOKE apply-generic/raise)
    (INVOKE install-basic-packages)
    
    (INVOKE GENERIC/=zero?)
    
    (INVOKE constructor/polynomial)
    
    ((lambda ()
       (INVOKE (export (prefix p: polynomial-package-internals^))
               install-polynomial-package)
       (INVOKE install-multivariate-polynomial-package)
       (p:populate-table/constructor-poly)
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
    
    ;; (get-tables table-show (lambda (-) nil))
    )
    
  (add 1 (make-scheme-number 3))
  (raise (raise (raise (raise 2))))

  (add
   (make-polynomial '(x) '(((2) 2)
                           ((1) 10)))
   (make-polynomial '(y) '(((2) 2)
                           ((1) 10))))
  
  (add (make-polynomial '(x y) '(((2 1) 2)
                                 ((1 2) 10)))
       (make-polynomial '(y x z) '(((0 1 3) 3)
                                   ((1 2 0) 7))))
  
  (add (make-polynomial '(x y z) `(((2 2 2) 3)
                                   ((2 1 0) 7)
                                   ((1 2 3) 2)
                                   ((0 0 0) ,(make-rational 3 4))))
       11)
  
  (add (make-polynomial '(x y z) '(((2 2 2) 3)
                                   ((2 1 0) 7)
                                   ((1 2 3) 2)))
       11)
  
  (add 1 2)
  (mul 4 5)

  (d "-----------------")

  (define p1 (make-polynomial '(x) '(((1) 1) ((0) -1))))

  (define p2 (make-polynomial '(x) '(((1) 1) ((0) 1))))
  
  (define p3 (make-polynomial '(x) '(((2)  1)
                                     ((1)  1)
                                     ((1) -1)
                                     ((0) -1))))
  
  (define p4 (make-polynomial '(x y) '(((2 0) 5)
                                       ((1 1) 2)
                                       ((0 1) 3))))
  
  (define p5 (make-polynomial '(y) '(((3) 3)
                                     ((0) 2))))
  (mul p1 p2)

  (add (mul p1 p2)
       (mul p1 p2))
  
  (add p3 p3)
  
  (mul p4 p5)
  
  'ok)

