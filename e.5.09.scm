
(load "aux/msim")

(set! DEBUG/EXECUTION true)

;;; in this exercise we do not want registers to keep pointers to
;;; labels any more.

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (:PRIMITIVE.constant (constant-exp-value exp)))
        ((label-exp? exp)
         (__d exp
              "TEST, ASSIGN and PERFORM"
              "operations should not be applied on labels --"
              exp)
         (newline)
         (let ((insts (lookup-label labels (label-exp-label exp))))
           (lambda ()
             (__d "runtime:" exp 'ERROR-no-label!!)
             insts)))
        ((register-exp? exp)
         (:PRIMITIVE.register (get-register machine (register-exp-reg exp))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (make-goto inst machine labels pc)
  ;; now it is impossible to jump to the value of a register
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (:GOTO-LABEL insts pc)))
          ((register-exp? dest)
           ;; assign should not put a value of label poiter into a
           ;; register, so goto should not jump to the label value
           ;; kept into a register
           (__d inst "GOTO should not jump to the value of a register")
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (__d "runtime -- goto jumps to reg " reg)
               ((set-contents! pc)
                (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define machine
  (make-machine
   '(a)
   `((/= ,(lambda (a b) (not (eq? a b)))))
   '(start
       (goto (label ::here))
     ::here
       (assign a (label ::there))
       (goto (reg a))
     ::there
       (test (op /=) (reg a) (label ::there))
       ;; will not branch as `a` is `::there`
       (branch (label ::there)))))

(set-register-contents! machine 'a 2)

(start machine)

(get-register-contents machine 'a)
