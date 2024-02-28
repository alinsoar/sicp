;;; -*- scheme -*-

(load "/work/sicp/sdf/manager/load")

(manage 'help)

((lambda (s) (s s (manage 'list-flavors)))
 (lambda (s flavors)
   (or (null? flavors)
       (begin (display (car flavors))
              (newline)
              (s s (cdr flavors))))))


(manage 'new-environment "combining-arithmetics")

(manage 'show-individual-tests #t)

(manage 'run-tests)
