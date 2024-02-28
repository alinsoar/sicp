
;; Using environment evaluator, one can obtain the same execution
;; speed but the loading time will be worse than in the case of a
;; compiler, because at each loading time the interpreter must do the
;; same preprocessing steps the compiler does only once..

;;; a difference between environment evaluator and bytecode evaluator
;;; is that the environment evaluator looks for all the operands in
;;; environment while the bytecode evaluator looks for some operands
;;; in bytecode itself.

;;; but in general everything can be compiled on the fly, then
;;; executed with the same speed.  The compilation takes time to
;;; repeat it at each loading.

;;; so one cannot eliminate the advantage of compilation.


