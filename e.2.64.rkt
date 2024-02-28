#lang racket/base

(require (submod "e.2.24.rkt" export))
(require (submod "e.2.63.rkt" export))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;;; (partial-tree ELTS N) returns a pair of the form
;;;
;;;   (TREE-LEFT . REMAINING-ELEMENTS).
;;;
;;; TREE-LEFT is a balanced tree containing first N elements of ELTS,
;;; keeping the same order, and REMAINING-ELEMENTS is a list that has
;;; the last (length ELTS)-N elements of ELTS, in the same order.
;;;
;;; In case that N = length(ELTS), PARTIAL-TREE returns
;;; ((BALANCED-TREE)), where BALANCED-TREE has the same elements from
;;; elts, in the same order.
;;;
;;; The order in which PARTIAL-TREE calls are made is described by the
;;; comments written in the code, and can be seen by instrumenting the
;;; evaluation to trace them.
;;;
;;; Briefly, PARTIAL-TREE is called for the first N elements, then for
;;; N/2 elements, then for N/4 elements, and finaly it will start to
;;; return balanced trees, with 1, 2, 4, 8 ... N/2, N elements.
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      ;; compute the half size of the list of elements
      (let ((left-size (quotient (- n 1) 2)))
        ;; LEFT-RESULT will be of this form after PARTIAL-TREE
        ;; returns:
        ;;
        ;;   (LEFT-TREE  REMAINING-LIST)
        ;;
        ;; LEFT-TREE will have the shape of a balanced tree with
        ;; LEFT-SIZE elements. REMAINING-LIST is the tail list of ELTS
        ;; without the elements of LEFT-TREE. The size of LEFT-TREE
        ;; minus the size of REMAINING-LIST is 0 or 1.
        (let ((left-result (partial-tree elts left-size)))
          ;; LEFT-TREE will have the shape of a balanced tree
          (let ((left-tree (car left-result))
                ;; non-left-elts is a list of the elements of the
                ;; right half of the ELTS. We need to make a balanced
                ;; tree with them.
                (non-left-elts (cdr left-result))
                ;; right-size is the length of the list
                ;; NON-LEFT-ELTS. In order to speed up the
                ;; computation, there is no need at this point to call
                ;; length(cdr NON-LEFT-ELTS), because we already know
                ;; how many elements we have in LEFT-TREE, and how
                ;; many elements there were in total.
                (right-size (- n (+ left-size 1))))
            ;; now we are going to process the right elements
            ;; recursively, in the same way as we initially called
            ;; (PARTIAL-TREE ELEMENTS (LENGTH ELEMENTS)). Now we make
            ;; a similar call, but only on (PARTIAL-TREE
            ;; REMAINING-ELEMENTS (LENGTH REMAINING-ELEMENTS)). We
            ;; keep out the car of REMAINING-ELEMENTS as the root of
            ;; the output tree,
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              ;; RIGHT-TREE is a balanced tree that has the elements
              ;; of (CDR REMAINING-ELEMENTS).
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                ;; finally, we return a pair of the form
                ;;
                ;; (ROOT-ELEMENT LEFT-TREE RIGHT-TREE)
                ;;
                ;; both LEFT-TREE and RIGHT-TREE have the form of a
                ;; balanced tree at this point. The cons and make-tree
                ;; and all the other non-recursive calls are processed
                ;; in constant time, and each element is processed
                ;; only once, so the total running time is O(n).
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))


;; (require racket/trace)

;; (trace partial-tree)

(module+ test

  (list->tree '(1 2 3 4 5 6 7 8 9))

  (tree-view (list->tree '(1 2 3 4 5 6 7 8 9) ))

  )

(module+ export
  (provide list->tree))
