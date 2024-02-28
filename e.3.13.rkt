#lang racket

(require "sicp.rkt")
(GETMOD 2 24)
(require scheme/mpair)
(require compatibility/mlist)

"
                     (make-cycle ...)
          ````````````````````````````````````````````````
          `                                              `
          `                                              `
          V                                              `
       +-----+-----+        +-----+-----+       +-----+-----+
       |  -  |     +------->|  -  |     +------>|  -  |  /  |
       +--+--+-----+        +--+--+-----+       +--+--+-----+
          |                    |                   |     ^
          a                    b                   c     |
                                                         |
                                                         |
                                  set-cdr! changes NIL here
                      (null? (cdr x)) is FALSE all the time
"

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)


(module+ test
  (require rackunit)
  (require racket/sandbox)
  
  (define z (make-cycle (mlist 'a 'b 'c)))
  
  "---"
  (mcar z)
  (mcar (mcdr z))
  (mcar (mcdr (mcdr z)))
  (mcar (mcdr (mcdr (mcdr z))))
  (mcar (mcdr (mcdr (mcdr (mcdr z)))))
  (mcar (mcdr (mcdr (mcdr (mcdr (mcdr z))))))
  (mcar (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr z)))))))
  (mcar (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr (mcdr z))))))))
  ":::"
  (tree-view-with-selectors-and-limit z 30 mcar mcdr mpair? mcons)
  
  "..."

  (infinite-loop (lambda () (last-pair z))
                 "infinite loop"
                 .1
                 "exp"))
