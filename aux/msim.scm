
(load "aux/sicp")

(define DEBUG/EXECUTION false)
(define DEBUG/PREPROCESSING false)

;; the public interface made of 4 procedures
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (let ((alloc-reg (machine 'allocate-register)))
      (for-each alloc-reg register-names))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  ((set-contents! (get-register machine register-name)) value)
  'done)
(define (start machine)
  (machine 'start))

;;;                                                                                               STACK
;;; ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *
;; (unmonitored) version from section 5.2.1
(define (make-stack1)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (lambda (message)
      (case message
        ('push       push)
        ('pop        (pop))
        ('initialize (initialize))
        (else (error "Unknown request -- STACK1" message))))))

;; monitored version from section 5.2.4
(define (make-stack2)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics ret)
      (newline)
      (display (list 'total-pushes  '= number-pushes 'maximum-depth '= max-depth))
      (newline)
      (ret number-pushes max-depth))
    (define (get-statistics ret)
      (ret number-pushes max-depth))
    (lambda (message)
      (case message
        ('push push)
        ('pop (pop))
        ('initialize (initialize))
        ('print-statistics print-statistics)
        ('get-statistics get-statistics)
        (else (error "Unknown request -- STACK2" message))))))

;;; Stack API
(define make-stack make-stack1)
(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

;;;                                                                                             MACHINE
;;; ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops `( (initialize-stack ,(lambda () (stack 'initialize)))
                      ;; only for stack2
                      (print-stack-statistics ,(lambda ()  (stack 'print-statistics)))
                      (print-stack-statistics/_/ ,(lambda ()  ((stack 'print-statistics) (lambda (_ __) 'ok))))))
          (register-table (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc insts))
                (execute)))))
      (lambda (message)
        (case message
          ('start                         ((set-contents! pc) the-instruction-sequence) (execute))
          ('install-instruction-sequence  (lambda (seq) (set! the-instruction-sequence seq)))
          ('allocate-register             allocate-register)
          ('get-register                  lookup-register)
          ('install-operations            (lambda (ops) (set! the-ops (append the-ops ops))))
          ('stack                         stack)
          ('operations                    the-ops)
          (else (error "Unknown request -- MACHINE" message)))))))
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;;;                                                                                    REGISTER/RUNTIME
;;; ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *

(define (make-register name)
  (let ((contents '*unassigned*))
    (lambda (message)
      (case message
        ('get contents)
        ('set (lambda (value) (set! contents value)))
        (else (error "Unknown request -- REGISTER" message))))))

;;; register API at runtime
(define (get-contents register)
  (register 'get))
(define (set-contents! register)
  (let ((setter (register 'set)))
    (lambda (value)
      (setter value))))

;;; machine access during (machine 'start)
;;; ~~ advance-pc
;;; ~~ set-contents!
;;; ~~ get-contents

(define :ASSIGN
  (lambda (target value-proc pc++)
    (let ((setter-target (set-contents! target)))
      (lambda ()
        (setter-target (value-proc))
        (pc++)))))
(define :TEST
  (lambda (flag condition-proc pc++)
    (let ((setter-flag (set-contents! flag)))
      (lambda ()
        (setter-flag (condition-proc))
        (pc++)))))
(define :BRANCH
  (lambda (flag insts pc pc++)
    (let ((setter-pc (set-contents! pc)))
      (lambda ()
        (if (get-contents flag)
            (setter-pc insts)
            (pc++))))))
(define :GOTO-LABEL
  (lambda (insts pc)
    (let ((setter-pc (set-contents! pc)))
      (lambda ()
        (setter-pc insts)))))
(define :GOTO-REGVAL
  (lambda (reg pc)
    (let ((setter-pc (set-contents! pc)))
     (lambda ()
       (setter-pc (get-contents reg))))))
(define :SAVE
  (lambda (stack reg pc++)
    (lambda ()
      (push stack (get-contents reg))
      (pc++))))
(define :RESTORE
  (lambda (stack reg pc++)
    (let ((setter-reg (set-contents! reg)))
      (lambda ()
        (setter-reg (pop stack))
        (pc++)))))
(define :PERFORM
  (lambda (action-proc pc++)
    (lambda ()
      (action-proc)
      (pc++))))
(define :APPLY-OP
  (lambda (op aprocs)
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
(define :PRIMITIVE.constant
  (lambda (c)
    (lambda () c)))
(define :PRIMITIVE.label
  (lambda (insts)
    (lambda () insts)))
(define :PRIMITIVE.register
  (lambda (r)
    (lambda () (get-contents r))))

(define (instruction-execution-proc inst)
  ;; return the instruction currently pointed by register PC
  (and DEBUG/EXECUTION (__d (caar inst)))
  (cdar inst))

(define (advance-pc pc)
  (let ((setter-pc (set-contents! pc)))
    (lambda ()
      (setter-pc (cdr (get-contents pc))))))

;;;                                                                                           ASSEMBLER
;;; ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  ;; returns a list of labels of the form   ( (label1 . instruct-sequence-pointed-by-label1)
  ;;                                          (label2 . instruct-sequence-pointed-by-label2) ... )
  ;; and a list of instructions of the form ( (instr1 . ()) (instr2 . ()) ... )
  ;;
  ;; the void cdr's of the list of instructions will be replaced by
  ;; pointers to runtime execution instructions of the form :ASSIGN,
  ;; etc
  ;;
  ;; the list of labels contain pointers to the list of instructions.

  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (receive insts (cons (make-label-entry next-inst insts) labels))
                              (receive (cons (make-instruction next-inst) insts) labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine pc flag stack ops)))
     insts)))

(define (make-instruction text)
  ;; set-instruction-execution-proc! will update the cdr with the
  ;; runtype procedure of the type :ASSIGN.
  ;;
  ;; the car part is ignored during the execution and is used only for
  ;; preprocessing by make-execution-procedure
  (cons text 'unset))

(define instruction-text car)
(define set-instruction-execution-proc! set-cdr!)
(define make-label-entry cons)

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

;;;                                                                                          PREPROCESS
;;; ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ *

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (and DEBUG/PREPROCESSING
       (__d "~~" inst ))
  (case (car inst)
    ('assign  (make-assign  inst machine labels ops pc))
    ('test    (make-test    inst machine labels ops flag pc))
    ('branch  (make-branch  inst machine labels flag pc))
    ('goto    (make-goto    inst machine labels pc))
    ('save    (make-save    inst machine stack pc))
    ('restore (make-restore inst machine stack pc))
    ('perform (make-perform inst machine labels ops pc))
    (else (error "Unknown instruction type -- ASSEMBLE" inst))))

;;; ASSIGN
(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (make-operation-exp value-exp machine labels operations
                               (lambda (val) val)
                               (lambda () (make-primitive-exp (car value-exp) machine labels)))))
      ;; execution procedure for assign
      (:ASSIGN target value-proc (advance-pc pc)))))
(define assign-reg-name cadr)
(define assign-value-exp cddr)

;;; CONDITIONAL -- SET FLAG
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (make-operation-exp condition machine labels operations
                        (lambda (condition-proc)
                          (:TEST flag condition-proc (advance-pc pc)))
                        (lambda ()
                          (error "Bad TEST instruction -- ASSEMBLE" inst)))))
(define test-condition cdr)

;;; CONDITIONAL -- BRANCH if FLAG is TRUE
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
          (:BRANCH flag insts pc (advance-pc pc)))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define branch-dest cadr)

;;; JUMP
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (:GOTO-LABEL insts pc)))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             (:GOTO-REGVAL reg pc)))
          (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))
(define goto-dest cadr)

;;; PUSH
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (:SAVE stack reg (advance-pc pc))))

;;; POP
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (:RESTORE stack reg (advance-pc pc))))
(define stack-inst-reg-name cadr)

;;; outside ACTION -- IO
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (make-operation-exp action machine labels operations
                        (lambda (action-proc) (:PERFORM action-proc (advance-pc pc)))
                        (lambda () (error "Bad PERFORM instruction -- ASSEMBLE" inst)))))
(define perform-action cdr)

;;; primitive
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp) (:PRIMITIVE.constant (constant-exp-value exp)))
        ((label-exp? exp)    (:PRIMITIVE.label (lookup-label labels (label-exp-label exp))))
        ((register-exp? exp) (:PRIMITIVE.register (get-register machine (register-exp-reg exp))))
        (else (error "Unknown expression type -- ASSEMBLE" exp))))

(define register-exp? (tagged-list? 'reg))
(define register-exp-reg cadr)

(define constant-exp? (tagged-list? 'const))
(define constant-exp-value cadr)

(define label-exp? (tagged-list? 'label))
(define label-exp-label cadr)

;;; OPERATION -- can appear in ASSIGN, PERFORM and in TEST-CONDITIONAL like that
;;; (test (op =) (reg n) (const 0))
;;; (assign n (op -) (reg n) (const 1))
;;; (perform (op print) (reg a))
(define (make-operation-exp exp machine labels operations succ fail)
  ;; looks for the procedure associated to a machine instruction like
  ;; (op X), preprocess each operand of the operator `X` and returns
  ;; the thunk that apply the operand on the operators ar runtime.
  (if (operation-exp? exp)
      (let ((op (lookup-prim (operation-exp-op exp) operations))
            (aprocs (map (lambda (e) (make-primitive-exp e machine labels))
                         (operation-exp-operands exp))))
        (succ (:APPLY-OP op aprocs)))
      (fail)))

(define (operation-exp? exp) ((tagged-list? 'op) (car exp)))
(define operation-exp-op cadar)
(define operation-exp-operands cdr)

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))
