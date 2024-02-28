
;;; Elementary Newton root finder

(define (root-newton f initial-guess tolerance)
  (let ((Df (derivative f)))
    (define (improve-guess xn)
      (- xn (/ (f xn) (Df xn))))
    (let loop ((xn initial-guess))
      (let ((xn+1 (improve-guess xn)))
        (if (close-enuf? xn xn+1 tolerance)
            xn+1
            (loop xn+1))))))


#|
;;; Limiting iterations if not converging
(define (root-newton f initial-guess tolerance #!optional maxiter)
  (let ((Df (derivative f)) 
        (maxiter (if (default-object? maxiter) 10 maxiter)))
    (define (improve-guess xn)
      (- xn (/ (f xn) (Df xn))))
    (let loop ((xn initial-guess) (n 0))
      (let ((xn+1 (improve-guess xn)))
        (if (close-enuf? xn xn+1 tolerance)
            xn+1
            (if (> n maxiter)
                #f
                (loop xn+1 (+ n 1))))))))
|#

;;; Example

(define (cs theta)
  (- (cos theta) (sin theta)))

(root-newton cs 0.5 1e-8)
'expect .7853981633974484
