#lang racket

(require "sicp.rkt")
(require "e.4.55.DB.rkt")
(GETMOD 4 55)

(module+ test

  (initialize-data-base
   (append a-data-base
           ;; TODO: comment each rule in detail.
           '(
             ;; INITIAL UNIFICATION
             (rule (reverse ?a ?b)
                   (reverse ?a        ()           ?b         () ) )
             
             ;; (A:D   ACC1     X:Y   ACC2) ==> (D     A:ACC1   Y     X:ACC2)
             ;; is the same as                X1   REV.X1   X2       REV.X2
             ;; taking into account that      X2 = REV.X1
             ;; which is                      X1   REV.X1   REV.X1   REV.REV.X1
             ;; simplifies to                 X1   REV.X1   REV.X1   X1
             (rule (reverse (?a . ?d) ?rev1        (?x . ?y) ?rev2)
                   (reverse ?d        (?a . ?rev1) ?y        (?x . ?rev2) ) )
             
             ;; X   A:REV.X   REV.X   A:X
             (rule (reverse ?x        (?a . ?r)    ?r         (?a . ?x) ) )
             
             ;; X   REV.X   REV.X   X
             (rule (reverse ?x        ?xr          ?xr        ?x) ) ) ) )

  (test/ '(
           (reverse (1 2 3) ?x)

           (reverse ?x (1 2 3) )

           (reverse (?x . ?a) (1 2 ?x) )

           ;; infinite loop
           ;; (reverse (?x . ?a) (1 . ?x))

           (reverse (x a b c) (c b ?a x) ) ) )

  'done)

