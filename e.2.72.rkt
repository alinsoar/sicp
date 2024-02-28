#lang racket

(require (submod "e.2.69.rkt" export))
(require (submod "e.2.24.rkt" export))
(require (submod "e.2.71.rkt" export))

(define alphabet '((A 1)
                   (B 2)
                   (C 4)
                   (D 8)
                   (E 16)
                   (F 32)
                   (G 64)
                   (H 128)
                   (I 256)
                   (J 512)
                   (K 1024)
                   (L 2048)
                   (M 4096)
                   ))

;;; after the 1st recursive call of `successive-merge`, we joined the
;;; (A . 1) and (B . 2) in a group (A B . 3) of weight frequence equal
;;; to 1+2=3. 3 less than 4.

;;; After the 2nd recursive call of `successive-merge`, we join the
;;; group (A B . 3) with the leaf (C . 4), and get the group of weight
;;; 7, (A B C . 7). 7 is less than 8, etc.

;;; In total, we call `successive-merge` for each member of the
;;; alphabet, so the number of elements of the alphabet.

;;; The code for the least frequent symbol is computed after N calls,
;;; and the code for the most frequent symbol is computed after N
;;; calls as well.

;;; However, the complexity is not O(N), because inside
;;; `successive-merge` adjoin-set is not in O(1). In total, the
;;; complexity is O(N^2).

(module+ test
  (tree-view
   (filter-tree
    (generate-huffman-tree alphabet)
    '())))

