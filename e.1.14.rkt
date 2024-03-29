#lang racket


(require racket/format)

(define (count-change amount)
  (cc amount 5 '()))

(define (display-tree depth amount kinds-of-coins)
  (if (null? depth)
      (printf "--~a__(~a : ~a)\n"
              ;; the nodes that bifurcate are marked with asterisk.
              ;; the terminals are marked with diez.
              (if (or (<= amount 0) (<= kinds-of-coins 0) ) "#" "*")
              amount
              kinds-of-coins)
      (begin
        (display
         (cond
          ((equal? depth '(1)) "  +")
          ((equal? depth '(2)) "  '")
          ((= 2 (car depth))   "   ")
          (else                "  |")))
        (display-tree (cdr depth) amount kinds-of-coins))))

(define (cc amount kinds-of-coins depth)
  (display-tree (reverse depth) amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1)
                     (cons '1 depth))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins
                     (cons '2 depth))))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 11)

;; the depth of the tree is N/min-coin+number-of-coins. If the minimal
;; coin is 1, then the depth is N.

;; space requirement : at each moment there are
;; on the stack only the nodes above the current
;; node, so the space complexity is maximum the
;; depth N/min-coin

;; time complexity. well, this is complex to compute exactly. One
;; can establis inductively an upper bound O(N^M).

;; Looking at the tree generated by 'display-tree, one can deduce
;; inductively the upper bound of O(N^M).

;; For (cc N 1) it makes 2*N calls of cc.
;; So, in order to compute (cc N 1) it needs O(N).

;; For (cc N 2) it makes N/{the value of the 2nd coin} calls to (cc N 1).
;; So, the time required to compute (cc N 2) is (N/5) * (2*N) = O(N^2)

;; To compute (cc N 3) it makes N/{the value of the 3rd coin}
;; calls to (cc N 2). So O(N^3)

;; Inductively, it has O(N^M) time complexity to compute
;; (cc N M). -- M is the number of coins.



