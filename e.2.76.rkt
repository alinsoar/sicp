#lang racket

(require (submod "e.1.06.rkt" export))
(require "sicp.rkt")

(define-signature GENERIC/mag/ang/real-part/imag-part/conj^
  (magnitude angle real-part imag-part conjugate))
(define-signature arith-complex/add/sub/mul/div^
  (add-complex sub-complex mul-complex div-complex))
(define-signature apply-generic^
  (apply-generic))
(define-signature constructors/complex^
  (make-from-real-imag make-from-mag-ang))

(define (apply-generic op arg) (arg op))

;;; constructor of rectangular representation of complex numbers
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          ((eq? op 'conjugate) (make-from-real-imag x (- y)))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)
;;; constructor of polar representation of complex numbers
(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          ((eq? op 'conjugate) (make-from-mag-ang m (- a)))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;;; when working with message-passing, to add a new operation at low
;;; level (inside the constructors) imply to supply that operation
;;; within each constructor.
(define-unit GENERIC/mag/ang/real-part/imag-part/conj@
  (import apply-generic^)
  (export GENERIC/mag/ang/real-part/imag-part/conj^)
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (conjugate z) (apply-generic 'conjugate z)))

;;; *** message-passing:
;;; 
;;; (new TYPE) to add a new representation of complex numbers means to
;;; add a new constructor that implements all low-level functions
;;; (like `angle`).
;;; 
;;; (new OPERATION) to add a new low level operation imply to supply
;;; that operation in each low level constructor, and a global
;;; definition of that operation (like `angle`).
;;; 
;;; (new ABSTRACTION) to add a new high level operation implies just
;;; to add that operation in global environment (like `add-complex`).
;;; 
;;; *** generic operations
;;; 
;;; (new TYPE) Adding a new type in generic operations means to add
;;; all the functions that involve that type in the global
;;; environment, and add a suffix (like `-polar`) to all low level
;;; functions of that type (like `real-part-rectangular`), such that
;;; not to conflict with the same operations from other type.  After
;;; that, one has to modify all the generic operations that are build
;;; on the low level constructors (like the generic `real-part`).  In
;;; all these functions, one need to add a new case in the switches
;;; that take different actions in function of each tag.
;;; 
;;; (new OPERATION) Adding a new operation means to define it in the
;;; global environment for each type, adding a suffix in the name of
;;; low level function that defines that type, then add a new generic
;;; operation that has a switch with a case for each possible tag.
;;; 
;;; (new ABSTRACTION) like in the case of message-passing, adding a
;;; new high level operation imply just to add that operation in
;;; global environment (like `add-complex`), and no other change.
;;; 
;;; *** data-directed style
;;; 
;;; (new TYPE) To add a new type means to write an `-install` function
;;; that completes a new column in the table of (operations . types).
;;; 
;;; (new OPERATION) To add a new operation means to add a new function
;;; defintion for that operation in each already existing `-install`
;;; function.
;;; 
;;; (new ABSTRACTION) This has the same complexity as in the other
;;; both cases.
;;; 
;;; Hence, generic operations on tagged data are the poorest in adding
;;; new operations and new types, while the other 2 possibilities have
;;; both the same complexity.  There are only 2 minor differences.
;;; The first difference is that in data-directed style the switching
;;; table is in the global environment, while in message-passing style
;;; the switching table is hidden in low level constructors (the
;;; constructor of each data type fills a column in message-passing
;;; style).  Other difference is that in the case of data-directed
;;; style the dispatcher is made in generic functions with a hash of
;;; the form of a list, as in `( tag1 tag2 tag3 )`, while in
;;; message-passing style there is no hash, because each object is a
;;; procedure itself, so it's not a list of tags, but the object
;;; itself.q
(define-unit arith-complex/add/sub/mul/div@
  (import constructors/complex^ apply-generic^
          GENERIC/mag/ang/real-part/imag-part/conj^)
  (export arith-complex/add/sub/mul/div^)
  (define add-complex
    (lambda (+)
      (lambda (z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2))))))
  (define sub-complex
    (lambda (-)
      (lambda (z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2))))))
  (define mul-complex
    (lambda (* +)
      (lambda (z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                           (+ (angle z1) (angle z2))))))
  (define div-complex
    (lambda (/ -)
      (lambda (z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                           (- (angle z1) (angle z2)))))))

(require (for-syntax racket/match))
(define-for-syntax (gn s)
  (string->symbol (string-append "GENERIC/" (symbol->string s) "@")))
(define-syntax (INVOKE-GENERIC unit)
  (let ((u (cadr (syntax->list unit))))
    (datum->syntax
     u
     `(define-values/invoke-unit/infer
        ,(gn (syntax->datum u))))))
(define-syntax (INVOKE syn)
  (match (syntax->list syn)
    [(list _ u)
     (datum->syntax
      syn
      `(define-values/invoke-unit/infer
         ,(string->symbol
           (string-append (symbol->string (syntax->datum u)) "@"))))]
    [(list _ export u)
     (datum->syntax
      syn
      `(define-values/invoke-unit/infer
         ,(syntax->datum export)
         ,(string->symbol
           (string-append (symbol->string (syntax->datum u)) "@"))))]
    [(list _ u import export)
     (datum->syntax
      syn
      `(define-values/invoke-unit
         ,(string->symbol
           (string-append (symbol->string (syntax->datum u)) "@"))
         ,(syntax->datum import)
         ,(syntax->datum export)))]))

(module+ test
  (INVOKE-GENERIC mag/ang/real-part/imag-part/conj)
  (INVOKE arith-complex/add/sub/mul/div)
  
  (define r (make-from-real-imag 10 10))
  (define w (make-from-mag-ang (magnitude r) (angle r)))
  (define t ((add-complex +) w w))
  (define x ((sub-complex -) w t))
  (define y ((div-complex / -) x w))
  (define z ((mul-complex * +) x x))
  (list
   (format "r=~a" (cons (real-part r) (imag-part r)))
   (format "w=~a" (cons (real-part w) (imag-part w)))
   (format "t=~a" (cons (real-part t) (imag-part t)))
   (format "x=~a" (cons (real-part x) (imag-part x)))
   (format "y=~a" (cons (real-part y) (imag-part y)))
   (format "z=~a" (cons (real-part z) (imag-part z)))
   (format "~~r=~a" (cons (real-part (conjugate r))
                          (imag-part (conjugate r))))
   (format "~~w=~a" (cons (real-part (conjugate w))
                          (imag-part (conjugate w)))))
  )

(module+ export
  (provide
   arith-complex/add/sub/mul/div@
   arith-complex/add/sub/mul/div^
   apply-generic^
   constructors/complex^
   GENERIC/mag/ang/real-part/imag-part/conj@
   INVOKE-GENERIC
   INVOKE
   GENERIC/mag/ang/real-part/imag-part/conj^))


