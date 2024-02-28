#lang racket

(require "sicp.rkt")

(GETMOD 4 55)

(module+ test
  (initialize-data-base
   '(
     (rule (last-pair (?x . ()) (?x . ())))
     (rule (last-pair (?a . ?b) (?c))
           (last-pair ?b (?c)))
     
     (rule (even zero))
     (rule (even (S ?r))
           (not (even ?r)))
     
     (rule (member ?item (?item . ?rest)))
     (rule (member ?item (?x . ?rest))
           (member ?item ?rest))

     (rule (= (?a) (?a)))
     (rule (= (?x . ?a) (?x . ?b))
           (= ?a ?b))

     (rule (mem ?item ?list)
           (= ?list (?item . ?_)))
     (rule (mem ?item ?list)
           (and (= ?list (?x . ?rest))
                (mem ?item ?rest)))

     (rule (add zero ?N ?N))
     (rule (add (S ?A) ?B (S ?R))
           (and (add ?A ?B ?R)))
     ))
  ;; ODD-EVEN
  (test/ '((even zero)
           (even (S zero))
           
           (COMM "---------------------------------------- LAST-PAIR")
           (last-pair (?x 3 4 5) (5))
           (last-pair (3) ?x)
           (last-pair (?x 3) (2))
           (last-pair (1 2 3) ?x)
           (last-pair (2 ?x) (3))
           (last-pair (11 22 33 44) (?x))
           (COMM "---------------------------------------- MEMBER")
           (member 10 (1 2 3))
           (member 22 (11 22 33))

           (COMM "---------------------------------------- =")

           (= (44) ())
           (= 44 (44))
           (= (44) (44))
           (= (44 55) (44 55))
           (= (44 55) (44 55 66))
           (= (44 55) (44 ?x))
           (= (44 55) (33 ?x))
           (= (?y 55) (33 ?x))
           (= (?y 55) (33 ?x))
           (= (?x ?y) (?a ?b))
           (= (?x ?y) (?a ?b ?c))
           (= (?x ?x 0) (1 ?b ?c))

           (COMM "---------------------------------------- MEM")
           
           (mem 11 (11 22 33))
           (mem 22 (11 22 33))
           (mem 44 (11))
           (mem 44 (44))
           (mem ?x (1 2 3 ?w 5 6))

           (COMM "---------------------------------------- PEANO")
           
           (add zero zero zero)
           (add zero (S zero) (S zero))
           (add zero (S (S zero)) (S (S zero)))
           (add (S zero) (S (S zero)) (S (S (S zero))))
           (add
            (S (S (S (S zero))))
            (S (S zero))
            (S (S (S (S (S (S zero)))))))
           (add
            (S (S (S (S zero))))
            (S (S zero))
            (S (S (S (S (S zero))))))
           (add
            ?x
            ?y
            (S (S (S (S (S zero))))))
           (add
            (S (S (S zero)))
            ?y
            (S (S (S (S (S zero))))))
           (add
            ?x
            (S (S (S zero)))
            (S (S (S (S (S zero))))))
           (add
            (S (S (S zero)))
            (S (S (S (S (S zero)))))
            ?r)
           (add
            ?x
            ?y
            (S (S (S (S (S (S (S (S zero))))))))))))
