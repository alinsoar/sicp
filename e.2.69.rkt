#lang racket

(require (submod "e.2.67.rkt" export))
(require (submod "e.2.24.rkt" export))

;; sets
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (let ((symbol (car pair))
              (weight (cadr pair)))
          (adjoin-set (make-leaf symbol weight)
                      (make-leaf-set (cdr pairs)))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (cond ((null? (cdr pairs)) (car pairs))
        (else (successive-merge
               (adjoin-set (make-code-tree (car pairs)
                                           (cadr pairs))
                           (cddr pairs))))))

;; (require racket/trace)
;; (trace successive-merge)

(module+ test

 (tree-view
  (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))))

(module+ export
  (provide generate-huffman-tree))
