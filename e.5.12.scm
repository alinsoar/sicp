
(load "aux/msim")

;; define the data paths of a machine by describing the registers and the operations.

;; To describe a register, we give it a name and specify the buttons
;; that control assignment to it.

;; We give each of these buttons a name and specify the source of the
;; data that enters the register under the button's control. (The
;; source is a register, a constant, or an operation.)

;; To describe an operation, we give it a name and specify its inputs
;; (registers or constants).

(define (extract/datapath controller-text)
  (let ((new (make-new-machine)))
    (extract-labels controller-text
                    (lambda (insts labels)
                      (for-each
                       (lambda (i)
                         (make-datapath i labels new))
                       (map instruction-text insts))
                      new))))

(define (make-new-machine)
  (let* ((datapath-uniq-instruction '())
         (datapath-reg-entry-point '())
         (datapath-stack-stored-registers '())
         (datapath-source-assignment '()))

    ;; build a list of all instructions, with duplicates removed,
    ;; sorted by instruction type (assign, goto, and so on).
    (define add-uniq-instruction!
      (lambda (instruction)
        (let ((type (car instruction)))
          (let ((v (assoc type datapath-uniq-instruction)))
            (if v
                (or (member instruction v)
                    (set! datapath-uniq-instruction
                          (cons (cons type (cons instruction (cdr v)))
                                (delete v datapath-uniq-instruction))))
                (set! datapath-uniq-instruction
                      (cons (list type instruction)
                            datapath-uniq-instruction)))))))

    (define add-goto-reg!
      (lambda (reg)
        (if (member reg datapath-reg-entry-point)
            'ok
            (set! datapath-reg-entry-point
                  (cons reg datapath-reg-entry-point)))))

    (define add-stack-stored-register!
      (lambda (reg)
        (if (member reg datapath-stack-stored-registers)
            'ok
            (set! datapath-stack-stored-registers
                  (cons reg datapath-stack-stored-registers)))))

    (define source-assignment-add!
      (lambda (reg source)
        (let ((v (assoc reg datapath-source-assignment)))
          (if v
              (or (member source v)
                  (set-cdr! v (cons source (cdr v))))
              (set! datapath-source-assignment
                    (cons (list reg source)
                          datapath-source-assignment))))))
    
    (lambda (message)
      (case message
        ('datapath-add!                       add-uniq-instruction!)
        ('datapath-get                        datapath-uniq-instruction)
        ('datapath-goto-reg-add!              add-goto-reg!)
        ('datapath-get-reg-entry-point        datapath-reg-entry-point)
        ('datapath-stack-stored-register-add! add-stack-stored-register!)
        ('datapath-get-stack-stored-registers datapath-stack-stored-registers)
        ('datapath-source-assignment-add!     source-assignment-add!)
        ('datapath-get-source-assignment      datapath-source-assignment)
        (else (error "Unknown request -- MACHINE" message))))))

(define (datapath--assign inst machine)
  (let ((target (assign-reg-name inst))
        (value-exp (assign-value-exp inst)))
    ((machine 'datapath-source-assignment-add!) target value-exp)
    ;; (__d "!assign!" target "," value-exp)
    
    ))

(define (datapath--test inst machine)
  (let ((condition (test-condition inst)))
    ;; (__d "!test!" condition)
    'ok
    ))

(define (datapath--branch inst machine)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        ;; (__d "!brach!" dest)
        'ok
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (datapath--save inst machine)
  (let ((reg (stack-inst-reg-name inst)))
    ((machine 'datapath-stack-stored-register-add!) reg)
    ;; (__d "!save!" reg)
    'ok
    ))

(define (datapath--restore inst machine)
  (let ((reg (stack-inst-reg-name inst)))
    ((machine 'datapath-stack-stored-register-add!) reg)
    ;; (__d "!restore!" reg)
    'ok
    ))

(define (datapath--goto inst machine labels)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             ;; (__d "!goto label!" insts)
             'ok
             ))
          ((register-exp? dest)
           (let ((reg (register-exp-reg dest)))
             ((machine 'datapath-goto-reg-add!) reg)
             ;; (__d "!goto reg!" reg)
             ))
          (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (datapath--perform inst machine)
  (let ((action (perform-action inst)))
    ;; (__d "!perform!" action)
    'ok
    ))

(define (make-datapath inst labels machine)
  (let ((instruction/name (car inst)))
    ((machine 'datapath-add!) inst)
    (case instruction/name
      ('assign  (datapath--assign  inst machine ))
      ('test    (datapath--test    inst machine ))
      ('branch  (datapath--branch  inst machine ))
      ('goto    (datapath--goto    inst machine labels))
      ('save    (datapath--save    inst machine ))
      ('restore (datapath--restore inst machine ))
      ('perform (datapath--perform inst machine ))
      (else (error "Unknown instruction type -- ASSEMBLE" inst)))))

(define fact
  (extract/datapath
   '(controller
     (assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done)))

(define fib
  (extract/datapath
   '(controller
     (assign continue (label ::fib-done))
     ::fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label ::immediate-answer))
     (save continue)
     (assign continue (label ::afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label ::fib-loop))
     ::afterfib-n-1
     (restore n)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label ::afterfib-n-2))
     (save val)                         ; will appear only once
     (save val)
     (save val)
     (goto (label ::fib-loop))
     ::afterfib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))
     ::immediate-answer
     (assign val (reg n))
     (perform (op test) test)
     (goto (reg continue))
     ::fib-done)))

(define test
  (lambda (machine)
    (for-each (lambda (x)
           (__d ":" (car x))
           (for-each (lambda (x)
                  (__d "\t" x))
                (cdr x)))
         (fib 'datapath-get))
    
    (__d "registers used to hold entry points"
         (machine 'datapath-get-reg-entry-point))

    (__d "registers that are saved or restored"
         (machine 'datapath-get-stack-stored-registers))

    (for-each
     (lambda (x)
       (__d "reg" (car x))
       (for-each
        (lambda (s)
          (__d "\t" s))
        (cdr x)))
     (machine 'datapath-get-source-assignment))
    'ok
    ))

(test fib)
(test fact)

