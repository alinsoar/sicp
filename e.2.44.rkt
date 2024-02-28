#lang racket

(require picturing-programs)

;;; the primitive graphical function are redefined here, such that the
;;; code from the textbook work with racket.  In mit-scheme there is
;;; no need of such redefinitions.
(define racket-beside beside)

(define flip-vert flip-vertical)
(define flip-horiz flip-horizontal)

(define (rogers) (bitmap "pict/william-barton-rogers.gif"))

(define (beside p1 p2)
  (let ((w1 (image-width p1))
        (w2 (image-width p2))
        (h1 (image-height p1))
        (h2 (image-height p2)))
    (let ((mw (min w1 w2))
          (mh (min h1 h2)))
      (racket-beside (scale/xy (/ mw w1) (/ mh h1) p1)
                     (scale/xy (/ mw w2) (/ mh h2) p2)))))

(define (below p1 p2)
  (let ((w1 (image-width p1))
        (w2 (image-width p2))
        (h1 (image-height p1))
        (h2 (image-height p2)))
    (let ((mw (min w1 w2))
          (mh (min h1 h2)))
      (above (scale/xy (/ mw w2) (/ mh h2) p2)
             (scale/xy (/ mw w1) (/ mh h1) p1)))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(module+ test
  (right-split (rogers) 2)

  (up-split (rogers) 2)

  (corner-split (rogers) 4)

  (flipped-pairs (rogers))

  (let ((painter (rogers)))
    (beside painter (below painter painter)))

  (let ((painter (rogers)))
    (below painter (beside painter painter)))

  (beside (rogers) (flip-vert (rogers)))

  (below (rogers) (flip-vert (rogers)))

  ;; the same as Maurits Cornelis Escher -- Square Limit
  (square-limit (rogers) 2))

