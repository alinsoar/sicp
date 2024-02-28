
;; In order to modify the order of evaluation one needs to modify the
;; CONSTRUCT-ARGLIST.

;; The default version defined in textbook accummulates a last to
;; first argument using the CONS. It computes it in this order:
    
    ;; PROC := EVALUATE (operator)
    ;; ARGL := ()
    ;; VAL = EVALUATE (last_arg)
    ;; ARGL := CONS (VAL, ARGL)
    ;; ...
    ;; VAL = EVALUATE (first_arg)
    ;; ARGL := CONS (VAL, ARGL)
    ;; CALL PROC
    
    
;; Had we evaluated the first argument first, and added it to ARGL
;; register via the CONS, we should have reversed the ARGS before to
;; call PROC, and it would have been slower to reverse ARGS before
;; each call.

;; Another idea that works as fast as last to first method is to use a
;; vector of length the number of arguments and fill the nth actual
;; argument after having computed it.  This also can be done with a
;; list and use SET-NTH!


    ;; PROC := EVALUATE (operator)
    ;; ARGL := MAKE-LIST (number of arguments, NIL)
    ;; VAL = EVALUATE (ith_arg)
    ;; SET-NTH! (ARGL, i, VAL)
    ;; VAL = EVALUATE (jth_arg)
    ;; SET-NTH! (ARGL, j, VAL)
    ;; ...
    ;; CALL PROC

;; and the order of evaluation of arguments is arbitrary and the speed
;; is the same as in the case of last-to-first method.

'ok