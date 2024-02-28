#lang racket


(define (make-mobile left right)
  (list left right))
(define left-branch car)
(define right-branch cadr)

(define (make-branch length structure)
  (list length structure))
(define branch-length car)
(define branch-structure cadr)

;;; this is the definition of the mobile from the given picture.
(define  w2  2)
(define  w3  3)
(define  w4  4)
(define  w5  5)
(define  w6  6)
(define  w7  7)
(define  w8  8)
(define  w9  9)
(define  w10 10)
(define  b4  (make-branch 6   w6 ))
(define  b8  (make-branch 1   w4 ))
(define  b9  (make-branch 1   w5 ))
(define  b10 (make-branch 1   w7 ))
(define  b11 (make-branch 1   w8 ))
(define  b12 (make-branch 3   w9 ))
(define  b13 (make-branch 1   w10))
(define  m5  (make-mobile b12 b13))
(define  m7  (make-mobile b8  b9 ))
(define  m8  (make-mobile b10 b11))
(define  b5  (make-branch 1   m8))
(define  b6  (make-branch 1   m5))
(define  b7  (make-branch 1   w3))
(define  b3  (make-branch 1   m7))
(define  m3  (make-mobile b4  b5 ))
(define  m4  (make-mobile b6  b7 ))
(define  b1  (make-branch 1   m3))
(define  b2  (make-branch 1   m4))
(define  m2  (make-mobile b2  b3 ))
(define  b0  (make-branch 1   m2))
(define  m1  (make-mobile b0  b1 ))

" each node is a mobile (M). At the left and the right of a mobile is
a branch (B) having a given length. At the end of each branch there is
either a weight (W) or another mobile (M).

                             M2                    M1                  M3
                             o__________B0_________o_________B1________o
                            / \                                       / \
                           /   \                                     /   \
                          /     \                                   /     \
                         /       \                                 /       \
                       B2        B3                              B4        B5 
                       /           \                             /           \
                      /             \                           /             \
                     /               \                         /               \
              M5   M4                 M7                      /                 M8 
              o_B6_o__B7_o       o_B8__o__B9_o               o             o_B10_o_B11_o
             /\          W3      W4          W5             W6             W7          W8
          B12  \B13
           /    \
          W9     W10
"

(define weight-structure? number?)

(define (total-weight mobile)
  (let ((lb (left-branch mobile))
        (rb (right-branch mobile)))
    (let ((ll (branch-length lb))
          (ls (branch-structure lb))
          (rl (branch-length rb))
          (rs (branch-structure rb)))
      (+ (* ll (if (weight-structure? ls)
                   ls
                   (total-weight ls)))
         (* rl (if (weight-structure? rs)
                   rs
                   (total-weight rs)))))))

(define (balanced? mobile)
  (define (iter mob)
    (let ((lb (left-branch mob))
          (rb (right-branch mob)))
      (let ((ll (branch-length lb))
            (ls (branch-structure lb))
            (rl (branch-length rb))
            (rs (branch-structure rb)))
        (let ((res-left (if (weight-structure? ls)
                            ls
                            (iter ls)))
              (res-right (if (weight-structure? rs)
                             rs
                             (iter rs))))
          (cond ((or (eq? false res-right)
                     (eq? false res-left))
                 false)
                ((= (* ll res-left) (* rl res-right))
                 (* 2 ll res-left))
                (else false))))))
  (number? (iter mobile)))

(define (total-weight mobile)
  (define (iter mob co)
    (let ((lb (left-branch mob))
          (rb (right-branch mob)))
      (let ((ll (branch-length lb))
            (ls (branch-structure lb))
            (rl (branch-length rb))
            (rs (branch-structure rb)))
        (cond ((and (weight-structure? ls)
                    (weight-structure? rs))
               (co (+ (* ll ls) (* rl rs))))
              ((weight-structure? ls)
               (iter rs
                     (lambda (x)
                       (co (+ x (* ll ls))))))
              ((weight-structure? rs)
               (iter ls
                     (lambda (x)
                       (co (+ x (* rl rs))))))
              (else
               (iter ls
                     (lambda (x)
                       (iter rs
                             (lambda (y)
                               (co (+ x y)))))))))))
  (iter mobile (lambda (x) x)))

(balanced? (make-mobile
            (make-branch 4 20)
            (make-branch 5 (make-mobile
                            (make-branch 4 2)
                            (make-branch 2 (make-mobile
                                            (make-branch 1 (make-mobile
                                                            (make-branch 1 1)
                                                            (make-branch 1 1)))
                                            (make-branch 1 (make-mobile
                                                            (make-branch 1 1)
                                                            (make-branch 1 1)))))))))

(total-weight m1)

;; part d

;;; The only change that we do is to selectors left-branch
;;; right-branch branch-length and branch-structure
(define (make-mobile left right)
  (cons left right))
(define left-branch car)
(define right-branch cdr)

(define (make-branch length structure)
  (cons length structure))
(define branch-length car)
(define branch-structure cdr)


