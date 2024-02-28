#lang racket

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;;; first scans in left or right, then appends the result from left
;;; and right. Each node is scanned once, and for each node there is
;;; an `append` that takes O(n). In total, O(n^2).
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

;;; first collect in right, then conses the current element in O(1) to
;;; the result from right, then pass to left with this value. At left,
;;; it will be added each node in constant time, at the left of the
;;; value collected from right. Each node is scanned once, and added
;;; to the result in constant time, so this takes O(n).
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(module+ export
  (provide make-tree
           entry
           left-branch
           right-branch
           tree->list-1
           tree->list-2))

(module+ test
 (tree->list-1 '(10 (5 (4 (3 (2 (1 (0 () ())
                                   ())
                                ())
                             ())
                          ())
                       (8 (6 ()
                             (7 () ()))
                          (9 () ())))
                    (20 (11 ()
                            (12 ()
                                (13 ()
                                    (14 ()
                                        (15 ()
                                            (16 ()
                                                (17 ()
                                                    (18 ()
                                                        (19 () ())))))))))
                        ())))

 (tree->list-2 '(10 (5 (4 (3 (2 (1 (0 () ())
                                   ())
                                ())
                             ())
                          ())
                       (8 (6 ()
                             (7 () ()))
                          (9 () ())))
                    (20 (11 ()
                            (12 ()
                                (13 ()
                                    (14 ()
                                        (15 ()
                                            (16 ()
                                                (17 ()
                                                    (18 ()
                                                        (19 () ())))))))))
                        ()))))




