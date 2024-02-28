#lang racket

(require (except-in (submod "e.2.07.rkt" export)
                    mul-interval))

;;; impossible cases, where the upper is smaller than lower
;; case \ values   z1-lower       z1-upper    |  z2-lower       z2-upper
;; A    1       |  +           |  +           |  +           |  +
;; IMPOSSIBLE   |  +           |  +           |  +    X      |  -    X
;; B    2       |  +           |  +           |  -           |  +
;; C    3       |  +           |  +           |  -           |  -
;; IMPOSSIBLE   |  +    X      |  -    X      |  +           |  +
;; IMPOSSIBLE   |  +    X      |  -    X      |  +    X      |  -    X
;; IMPOSSIBLE   |  +    X      |  -    X      |  -           |  +
;; IMPOSSIBLE   |  +    X      |  -    X      |  -           |  -
;; D    4       |  -           |  +           |  +           |  +
;; IMPOSSIBLE   |  -           |  +           |  +    X      |  -    X
;; E    5       |  -           |  +           |  -           |  +
;; F    6       |  -           |  +           |  -           |  -
;; G    7       |  -           |  -           |  +           |  +
;; IMPOSSIBLE   |  -           |  -           |  +    X      |  -    X
;; H    8       |  -           |  -           |  -           |  +
;; I    9       |  -           |  -           |  -           |  -
(define (mul-interval z1 z2)
  (let ((z1-low (lower-bound z1))
        (z1-up (upper-bound z1))
        (z2-low (lower-bound z2))
        (z2-up (upper-bound z2)))
    ;; patterns for clustering in function of the signs of z2.  ++,
    ;; -+, --; -- is not explicitly defined, as it belongs to
    ;; else-clauses in cases C F I.
    (define (++?) (positive? z2-low))
    (define (-+?) (positive? z2-up))
    ((lambda (bounds)
       (make-interval (car bounds) (cdr bounds)))
     (if (positive? z1-low)             ; case A,B,C -- D,E,F,G,H,I
         (cond ((++?)                   ; case A
                (cons (* z1-low z2-low) (* z1-up  z2-up)))
               ((-+?)                   ; case B
                (cons (* z1-up  z2-low) (* z1-up  z2-up)))
               (else                    ; case C
                (cons (* z1-up  z2-low) (* z1-low z2-up))))
         (if (positive? z1-up)          ; case D,E,F -- G,H,I
             (cond ((++?)               ; case D
                    (cons (* z1-low z2-up) (* z1-up z2-up)))
                   ((-+?)               ; case E
                    (cons (min (* z1-low z2-up) (* z1-up z2-low))
                          (max (* z1-low z2-low) (* z1-up z2-up))))
                   (else                ; case F
                    (cons (* z1-up z2-low) (* z1-low z2-up))))
             (cond ((++?)               ; case G
                    (cons (* z1-low z2-up) (* z1-up z2-low)))
                   ((-+?)               ; case H
                    (cons (* z1-low z2-up) (* z1-low z2-low)))
                   (else                ; case I
                    (cons (* z1-up z2-up) (* z1-low z2-low)))))))))


(module+ test
  (define z1 (make-interval 10 20))

  (mul-interval z1 (make-interval  1  2)) ; case A
  (mul-interval z1 (make-interval -1  2)) ; case B
  (mul-interval z1 (make-interval -2 -1)) ; case C

  (define z2 (make-interval -10 20))

  (mul-interval z2 (make-interval  1  2)) ; case D
  (mul-interval z2 (make-interval -1  2)) ; case E
  (mul-interval z2 (make-interval -2 -1)) ; case F

  (define z3 (make-interval -10 -20))

  (mul-interval z3 (make-interval  1  2)) ; case G
  (mul-interval z3 (make-interval -1  2)) ; case H
  (mul-interval z3 (make-interval -2 -1))) ; case I


