#lang racket

(require scheme/mpair)
(require compatibility/mlist)
(require "sicp.rkt")

;;; 
;;; This code abstracts the table data structure, and isolates the
;;; backbone, such that it can be passed as an external module.
;;;
;;; Here, I implemented a backbone as a BST, and a backbone as a
;;; simple list. Each backbone must be initialized using a comparator
;;; that will make an order on the objects that are inserted in the
;;; backbone.
;;; 
;;; If we have a backbone that, instead of a binary tree, uses a
;;; general tree, the algorithm looks very similar to a trie.
;;; 
;;; A valid backbone module MUST respond to the messages "search",
;;; "insert!", and the backbone constructor MUST receive a comparator
;;; for establishing an order for the keys. For example, if the
;;; objects are integers, the eq? or equal? or = can be used for its
;;; definition (because an integer, not being changeable, it's
;;; represented the same as a pointer).
;;; 
;;; When used to implement tables, the backbone module will have items
;;; in the form (KEY . VALUE). Value can be either any value on a
;;; leaf, or another table entry, in case that KEY is part of a
;;; multidimensional key.
;;; 
;;; Here is an example of table indexes using symbolic keys `a...z`
;;; and having 5 values V1 ... V5. On the leaves are stored only the
;;; values, but on the picture below I maked the keys as well and the
;;; value associated with each leaf in the form (keys value).
;;; 
;;; The KEYS are considered sorted objects (K1 K2 ... Kn). We cannot
;;; index using non-sorted objects, like the set {K1 ... Kn}, because
;;; the algorithm to compare 2 sets cannot be done in constant time,
;;; hence there would be impossible to insert and lookup for items in
;;; constant time if we had allowed nonsorted sets as keys.
;;; 
;;;                                *root-table*  
;;;                                ,-----o-----,
;;;                  c      ,-----'             '-----,       e
;;;                  o-----'                           '----- o
;;;                 / \                                      / \
;;;                /   \                                    /   \
;;;               /     \                                  /     \
;;;              /       \                                /       \
;;;             /         o k                            /         \
;;;       (c a V1)       / \                       (e b V4)    (e w V5)
;;;                     /   \
;;;                    /     \
;;;                   /       \
;;;                  /         \
;;;          (c k a V2)  (c k s V3)
;;;
;;; In this example of table, the KEY (c k a) is associated with the value V2.



;;; GENERIC COMPARATOR
(define id (lambda (x) x))
(define generic-comparator
  (lambda (< >)
    (lambda (structure)
      (lambda (x y)
        "if X>Y the comparator MUST return a POSITIVE NUMBER, else if
X<Y the comparator MUST return a NEGATIVE NUMBER, and ZERO for
equality. Ignore invalid arguments."
        (and x
             y
             (let ((x (structure x))
                   (y (structure y)))
               (cond ((> x y) 1)
                     ((< x y) -1)
                     (else 0))))))))
(define comparator-symbols ((generic-comparator string<? string>?) symbol->string))
(define comparator-integers ((generic-comparator < >) id))

;;; GENERIC BINARY TREE
(define (make-binary-tree comparator)
  (define (make-node item) (mlist item false false))
  (define (get-item node) (mcar node))
  (define (get-left node) (mcar (mcdr node)))
  (define (get-right node) (mcar (mcdr (mcdr node))))
  (define (set-item! node item) (set-mcar! node item))
  (define (set-left! node left) (set-mcar! (mcdr node) left))
  (define (set-right! node right) (set-mcar! (mcdr (mcdr node)) right))
  (let ((root (make-node #f)))
    (define (search item node)
      (let ((k (comparator (get-item node) item))
            (left (get-left node))
            (right (get-right node)))
        (cond ((not node) false)
              ((not k) false)
              ((< 0 k) (if left
                           (search item left)
                           false))
              ((> 0 k) (if right
                           (search item right)
                           false))
              (else (mcar node) ) ) ) )
    (define (insert! new-item node)
      (let ((k (comparator (get-item node) new-item))
            (left (get-left node))
            (right (get-right node))
            (value (get-item node)))
        (cond ((not k)
               '(d "; this must be the root node")
               (set-item! node new-item) )
              ((< 0 k) (if left
                           (insert! new-item left)
                           (begin
                             '(d "; insert at left of " value)
                             (set-left! node (make-node new-item)))))
              ((> 0 k) (if right
                           (insert! new-item right)
                           (begin
                             '(d "; insert at right of " value)
                             (set-right! node (make-node new-item)))))
              (else
               '(d "replace old value")
               (set-item! node new-item)
               ) ) ) )
    (define (show tree)
      (mlist->list (mmap (lambda (x) (if (mpair? x) (show x) x))
                         tree)))
    (define (dispatch-tree m)
      (cond ((eq? m 'search) (lambda (x) (search x root)))
            ((eq? m 'insert!) (lambda (x) (insert! x root) ' ok))
            ((eq? m 'display) (cons "TREE:" (show root)))
            ((eq? m 'get-root) root)
            (else (error "Unknown message for bin tree /" m) ) ) )
    dispatch-tree ) )
(define (int-binary-tree)
  (make-binary-tree comparator-integers))
(define (sym-binary-tree)
  (make-binary-tree comparator-symbols))
;;; cons-binary-tree is more complicated, it is a binary tree having
;;; entries of the form (KEY . VALUE), and we sort the entries using
;;; KEY. the `comparator` is used to make an order over the keys.
(define (cons-binary-tree comparator)
  (make-binary-tree
   (lambda (x y) (and x y (comparator (car x) (car y))))))
(define (cons-list-records comparator)
  (make-list-records
   (lambda (x y) (and x y (comparator (car x) (car y))))))

;;; GENERIC LIST.  We can make tables with backbone as list instead of
;;; binary tree. IMPORTANT: THIS IS NO MORE TESTED; as time as a data
;;; answers correctly to the messages "insert!" and "search" we can
;;; use it instead of the generic binary tree.
(define (make-list-records comparator)
  (let ((records (mlist)))
    (define (same-key? x y) (= 0 (comparator x y)))
    (define (show rec)
      (cond ((and rec
                  (number? (mcdr rec)))
             (display rec))
            (else (rec 'show))))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar records))
             (mcar records))
            (else (assoc key (mcdr records)))))
    (define (insert-list! new-entry)
      (set! records (mcons new-entry records) ) )
    (define (dispatch-list m)
      (cond ((eq? m 'insert!) (lambda (k) (insert-list! k)))
            ((eq? m 'search) (lambda (x)
                               (assoc x records)))
            ((eq? m 'show) (cons "RECORDS:" records))
            (else (error "unknown message /" m))))
    dispatch-list) )

;;; GENERIC TABLES with multidimensional keys
(define (make-table cons-backbone comparator)
  (lambda (table-name)
    (let ((backbone (cons-backbone comparator)))
      (define (search-backbone key) ((backbone 'search) key))
      (define (insert-backbone! key value) ((backbone 'insert!) (cons key value)))
      (define table? (lambda (t) (and (procedure? t) (eq? (t 'type) 'table))))
      (define (search keys)
        (let ((r (search-backbone keys)))
          (cond ((null? (cdr keys)) r)
                ((table? (cdr r))
                 (((cdr r) 'lookup-proc) (cdr keys)))
                (else (cons 'failure keys r)))))
      (define (insert! keys value)
        (let ((r (search-backbone keys)))
          (cond ((null? (cdr keys))
                 (insert-backbone! (car keys) value))
                ((and r (table? (cdr r)))
                 (((cdr r) 'insert-proc!) (cdr keys) value))
                (r (let ((new-table ((make-table cons-backbone comparator)
                                     (car keys))))
                     (insert-backbone! (car keys) new-table)
                     ((new-table 'insert-proc!) (cdr keys) value)))
                (else (error "todo")))
          'ok))
      (define (dispatch-table m)
        (cond ((eq? m 'lookup-proc) search)
              ((eq? m 'insert-proc!) insert!)
              ((eq? m 'get-name) table-name)
              ((eq? m 'type) 'table)
              ((eq? m 'get-backbone) backbone)
              (else (error "Unknown operation on table /" m))))
      dispatch-table)))
(define (get table keys)       ((table 'lookup-proc) keys))
(define (put table keys value) ((table 'insert-proc!) keys value))

;;; TEST UNIT
(module+ test
  (require rackunit)
  "---"
  (define bt1 (int-binary-tree))
  (define bt2 (sym-binary-tree))
  (define bt3 (cons-binary-tree comparator-symbols))
  (define bt4 (cons-binary-tree comparator-integers))
  (define table1 ((make-table cons-binary-tree comparator-symbols) "*root-table*"))
  (define table2 ((make-table cons-list-records comparator-integers) "*root-table*"))
  
  "********** first, test the binary trees having integers as entries"
  (begin
    bt1
    (bt1 'display)
    ((bt1 'insert!) 10)
    (bt1 'display)
    ((bt1 'insert!) 20)
    ((bt1 'insert!) 5)
    ((bt1 'insert!) 30)
    (bt1 'display)
    (set! bt1 nil))
  "********** second, test the binary trees having symbols as entries"
  (begin
    bt2
    (bt2 'display)
    ((bt2 'insert!) 'a)
    (bt2 'display)
    (test-case
     "exp"
     (check-exn exn:fail? (lambda () ((bt2 'insert!) 20)))
     "it is an error to insert numbers in a bt with a comparator for symbols")
    ((bt2 'insert!) 'b)
    ((bt2 'insert!) 'x)
    ((bt2 'insert!) 's)
    ((bt2 'insert!) 'b)
    (bt2 'display)
    ((bt2 'search) 'x)
    ((bt2 'search) 'w)
    ((bt2 'search) 's)
    (set! bt2 nil))
  "********** third, test the binary trees having entries (SYM-key, value)"
  (begin
    bt3
    (bt3 'display)
    (bt3 'insert!)
    ((bt3 'insert!) (cons 'a "*A"))
    (bt3 'display)
    ((bt3 'insert!) (cons 'a "*AA"))
    (bt3 'display)
    (test-case
     "exp"
     (check-exn exn:fail? (lambda () ((bt2 'insert!) '(20 . a))))
     "it is an error to insert numbers in a bt with a comparator for symbols")
    ((bt3 'insert!) '(b . a))
    ((bt3 'insert!) '(x . 20))
    ((bt3 'insert!) '(s . "S"))
    (bt3 'display)
    ((bt3 'insert!) '(b . "*B"))
    (bt3 'display)
    ((bt3 'search) '(x))
    ((bt3 'search) '(w))
    ((bt3 'search) '(s))
    ((bt3 'search) '(b)))
  "********** last for trees, test the binary trees having entries (INT-key, value)"
  (begin
    bt4
    (bt4 'display)
    (bt4 'insert!)
    ((bt4 'insert!) (cons 9 "*A"))
    (bt4 'display)
    ((bt4 'insert!) (cons 11 "*AA"))
    (bt4 'display)
    (test-case
     "exp"
     (check-exn exn:fail? (lambda () ((bt2 'insert!) '(a . "A"))))
     "it is an error to insert symbols in a bt with a comparator for numbers")
    ((bt4 'insert!) '(11 . a))
    ((bt4 'insert!) '(12 . 20))
    ((bt4 'insert!) '(14 . "S"))
    (bt4 'display)
    ((bt4 'insert!) '(11 . "*B"))
    (bt4 'display)
    ((bt4 'search) '(11))
    ((bt4 'search) '(12))
    ((bt4 'search) '(14)))

  "********** next, we test tables with keys of list of symbols and backbone a BT"
  (begin
    table1
    (table1 'get-name)
    ((table1 'insert-proc!) '(q) "*Q")
    ((table1 'get-backbone) 'display)
    ((table1 'insert-proc!) '(q) 'b)
    ((table1 'get-backbone) 'display)
    ((table1 'insert-proc!) '(w) "*w")
    ((table1 'get-backbone) 'display)
    ((table1 'insert-proc!) '(q u) "*QU")
    ((table1 'get-backbone) 'display)
    "///"
    ((table1 'get-backbone) 'get-root)
    (((table1 'get-backbone) 'search) '(q))
    "Q =>" (cdr (((table1 'get-backbone) 'search) '(q)))
    (((cdr (((table1 'get-backbone) 'search) '(q))) 'get-backbone) 'get-root)
    ((((cdr (((table1 'get-backbone) 'search) '(q))) 'get-backbone) 'search) '(u))
    "///"
    ((table1 'insert-proc!) '(q v) "*QV")
    ((table1 'get-backbone) 'get-root)
    (((table1 'get-backbone) 'search) '(q))
    "Q =>" (cdr (((table1 'get-backbone) 'search) '(q)))
    (((cdr (((table1 'get-backbone) 'search) '(q))) 'get-backbone) 'get-root)
    ((((cdr (((table1 'get-backbone) 'search) '(q))) 'get-backbone) 'search) '(v))
    "+++*+++"
    ((table1 'lookup-proc) '(aaa))
    ((table1 'lookup-proc) '(q))
    ((table1 'lookup-proc) '(q aaa))
    ((table1 'lookup-proc) '(q u))
    ((table1 'lookup-proc) '(q v))
    ((table1 'insert-proc!) '(q v) "*Q newV")
    ((table1 'lookup-proc) '(q v)))

  "********** finally, we test tables with keys of list of symbols and backbone a list"
  "**** THIS TEST PROVES that it does not matter how the tree structure is implemented;"
  "it works any data abstraction as time as it correctly answers to SEARCH and INSERT!"
  
  table2
  (table2 'get-name)
  (table2 'get-backbone)
  ((table2 'get-backbone) 'show)
  ((table2 'insert-proc!) '(11) "*Q")
  ((table2 'get-backbone) 'show)
  ((table2 'lookup-proc) '(11))
  ((table2 'insert-proc!) '(13) "*R")
  ((table2 'get-backbone) 'show)
  ((table2 'insert-proc!) '(13 15) "*S")
  ((table2 'get-backbone) 'show)
  ((table2 'lookup-proc) '(13))
  (((cdr ((table2 'lookup-proc) '(13))) 'get-backbone) 'show)
  )

