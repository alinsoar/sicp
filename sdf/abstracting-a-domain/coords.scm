
;;;; Coordinates

(define (make-coords column row)
  (cons column row))

(define (get-column coords)
  (car coords))

(define (get-row coords)
  (cdr coords))

(define (coords=? c1 c2)
  (and (= (get-column c1) (get-column c2))
       (= (get-row c1) (get-row c2))))

#|;;; Used only to define piece<? in piece.scm
(define (coords<? c1 c2)
  (or (< (get-column c1) (get-column c2))
      (and (= (get-column c1) (get-column c2))
           (< (get-row c1) (get-row c2)))))
|#

(define (coords+ c1 c2)
  (make-coords (+ (get-column c1) (get-column c2))
               (+ (get-row c1) (get-row c2))))

(define (coords- c1 c2)
  (make-coords (- (get-column c1) (get-column c2))
               (- (get-row c1) (get-row c2))))

(define (offset* offset scale)
  (make-coords (* (get-column offset) scale)
               (* (get-row offset) scale)))

(define (offset/ offset scale)
  (make-coords (/ (get-column offset) scale)
               (/ (get-row offset) scale)))

(define (offset->direction offset)
  (make-coords (sign (get-column offset))
               (sign (get-row offset))))

(define forward-direction
  (make-coords 0 1))

(define backward-direction
  (make-coords 0 -1))

(define left-direction
  (make-coords -1 0))

(define right-direction
  (make-coords 1 0))

(define forward-diagonal-directions
  (list (make-coords 1 1)
        (make-coords -1 1)))

(define backward-diagonal-directions
  (list (make-coords 1 -1)
        (make-coords -1 -1)))

(define diagonal-directions
  (append forward-diagonal-directions
          backward-diagonal-directions))