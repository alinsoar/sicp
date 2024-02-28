
;;;; Pieces

(define-record-type <piece>
    (make-piece color type coords)
    piece?
  (color piece-color)
  (type piece-type)
  (coords piece-coords))

(define-record-printer <piece>
  (lambda (piece)
    ;; Don't reverse the order of the first two here.
    ;; Checkers terminology makes it offensive then.
    (list (piece-type piece)
          (piece-color piece)
          (piece-coords piece))))

(define (piece=? p1 p2)
  (and (eq? (piece-color p1) (piece-color p2))
       (eq? (piece-type p1) (piece-type p2))
       (coords=? (piece-coords p1) (piece-coords p2))))

#|;;; Never used in any of our code!
(define (piece<? p1 p2)
  (coords<? (piece-coords p1) (piece-coords p2)))
|#

(define (piece-move piece coords)
  (make-piece (piece-color piece)
              (piece-type piece)
              coords))

(define (piece-new-type piece type)
  (make-piece (piece-color piece)
              type
              (piece-coords piece)))

(define (same-color? p1 p2)
  (eq? (piece-color p1) (piece-color p2)))