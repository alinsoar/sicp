;;; (MIT TOOL)
;;; 
;;; Tool EVAL is identical to Scheme EVAL, except for adding three new
;;; forms:
;;; 
;;;    (DEFINE-GENERIC-FUNCTION name)
;;;    (DEFINE-METHOD generic-function (params-and-classes) . body)
;;;    (DEFINE-CLASS superclass . slot-names)  returns the class
;;;    (MAKE class-for-this-object . slot-values)

(define (tool-eval exp env)
  (cond
   ((self-evaluating? exp)   exp)
   ((quoted? exp)            (text-of-quotation exp))
   ((get-environment? exp)   env)
   ((variable? exp)          (lookup-variable-value exp env))
   ((definition? exp)        (eval-definition exp env))
   ((assignment? exp)        (eval-assignment exp env))
   ((lambda? exp)            (make-procedure exp env))
   ((conditional? exp)       (eval-cond (clauses exp) env))
   ((or-conditional? exp)    (eval-or-cond (or-clauses exp) env))
   ((case? exp)              (eval-case (case-key exp)
                                        (case-clauses exp) env))
   ((genfun-definition? exp) (eval-generic-function-definition exp env))
   ((method-definition? exp) (eval-define-method exp env))
   ((class-definition? exp)  (eval-define-class exp env))
   ((instance-creation? exp) (eval-make exp env))
   ((def-class? exp)         (eval-def-class exp env))
   ((let? exp)               (tool-eval (let->combination exp) env))
   ((restart-tool? exp)      (do-restart))
   ((application? exp)       (tool-apply
                              (tool-eval (operator exp) env)
                              (map (lambda (operand)
                                     (tool-eval operand env))
                                   (operands exp))))
   (else (raise-error "Unknown expression type -- EVAL >> " exp))))

(define (tool-apply procedure arguments)
  (cond
   ;; PRIMITIVES
   ((primitive-procedure? procedure)
    (apply-primitive-procedure procedure arguments))
   ;; COMBINATIONS
   ((compound-procedure? procedure)
    (eval-sequence
     (procedure-body procedure)
     (extend-environment (parameters procedure)
                         arguments
                         (procedure-environment procedure))))
   ;; GENERIC FUNCTIONS -- POLYMORPHISMS
   ((generic-function? procedure)
    (apply-generic-function procedure arguments))
   ;; 
   (else (raise-error "Unknown procedure type -- APPLY"))))

;;; To apply a generic function, we look at the generic function and
;;; the arguments to decide which methods are applicable, and then run
;;; the first (most specific) applicable method.
(define (apply-generic-function generic-function arguments)
  (let ((methods (compute-applicable-methods-using-classes
                  generic-function
                  (map class-of arguments))))
    (if (null? methods)
        (raise-error "No method found -- APPLY-GENERIC-FUNCTION")
        (tool-apply (method-procedure (car methods)) arguments))))

;;; For the given generic function, get the applicable methods
;;; given the classes of the arguments.
(define (compute-applicable-methods-using-classes
         generic-function classes)
  (sort
   (filter
    (lambda (method)
      (method-applies-to-classes? method classes))
    (generic-function-methods generic-function))
   method-more-specific?))

;;; see if each supplied argument class is a subclass of the
;;; corresponding class required by the method specializer
(define (method-applies-to-classes? method classes)
  (define (check-classes supplied required)
    (cond ((and (null? supplied) (null? required)) true)
          ;; something left over, so number of arugments does not match
          ((or (null? supplied) (null? required)) false)
          ((subclass? (car supplied) (car required))
           (check-classes (cdr supplied) (cdr required)))
          (else false)
          ))
  (check-classes classes (method-specializers method)))

;;; We consider method1 to be more specific than method2 if each class
;;; prescribed for method1 is a subclass of the corresponding class
;;; prescribed for method2.  It is not clear whether this is the right
;;; thing.  (CLOS does something more complicated than this.)
(define (method-more-specific? method1 method2)
  (define (check-classes c1 c2)
    (cond ((and (null? c1) (null? c2)) true)
          ((or (null? c1) (null? c2))
           (raise-error "Bug: method lists not of same length"))
          ((subclass? (car c1) (car c2))
           (check-classes (cdr c1) (cdr c2)))
          (else false)))
  (check-classes (method-specializers method1)
                 (method-specializers method2)))

;;; An object is either an instance of an ordinary TOOL class, or else
;;; a built-in class.  The built-in classes are predefined for various
;;; Scheme objects (numbers, lists, ...).
(define (class-of object)
  (if (standard-instance? object)
      (instance-class object)
      (built-in-class object)))

;;; See if class1 is a subclass of class2.
(define (subclass? class1 class2)
  (or (eq? class1 class2)
      (memq class2 (class-ancestors class1))))

;;; Evaluation methods for the new TOOL expressions

;;; New special form (DEFINE-GENERIC-FUNCTION) .  This just returns an
;;; empty generic function.
(define (eval-generic-function-definition exp env)
  (let ((name (generic-function-definition-name exp)))
    (let ((val (make-generic-function name)))
      (define-variable! name val env)
      (list 'defined 'generic 'function: name))))

;;; New special form
;;; 
;;;     (DEFINE-METHOD generic-function params-and-classes . body)
;;; 
;;; This adds the method to the generic function for the appropriate
;;; classes.
;;; 
;;; params-and-classes is a list of elements ((p1 class1)...(pn classn))
;;; Body is the body for a procedure whose parameters are (p1...pn)
(define (eval-define-method exp env)
  (let ((gf (tool-eval (method-definition-generic-function exp) env)))
    (if (not (generic-function? gf))
        (raise-error "Unrecognized generic function -- DEFINE-METHOD >> "
                     (method-definition-generic-function exp))
        (let ((params (method-definition-parameters exp)))
          (install-method-in-generic-function
           gf
           (map (lambda (p) (paramlist-element-class p env))
                params)
           (make-procedure (make-lambda-expression
                            (map paramlist-element-name params)
                            (method-definition-body exp))
                           env))
          (list 'added 'method 'to 'generic 'function:
                (generic-function-name gf))))))

;;; Install the method in the generic function: The method consists of
;;; specializers and a procedure.
(define (install-method-in-generic-function gf specializers proc)
  (let ((method (make-method specializers proc)))
    (add-method-to-generic-function! method gf)))
(define (paramlist-element-class p env)
  (let ((class (tool-eval (paramlist-element-class-name p) env)))
    (if (class? class)
        class
        (raise-error "Unrecognized class -- DEFINE-METHOD >> "
                     class))))

;;; New special form (DEFINE-CLASS name superclass . slots)
(define (eval-define-class exp env)
  (let ((superclass (tool-eval (class-definition-superclass exp)
                               env)))
    (if (not (class? superclass))
        (raise-error "Unrecognized superclass -- MAKE-CLASS >> "
                     (class-definition-superclass exp))
        (let ((name (class-definition-name exp))
              (all-slots (collect-slots
                          (class-definition-slot-names exp)
                          superclass)))
          (d "C:" name "U:" superclass "S:" all-slots)
          (let ((new-class (make-class name superclass all-slots)))
            (define-variable! name new-class env)
            (list "defined class:" name))))))
(define (collect-slots slot-names superclass)
  (let ((superclass-slots (class-slot-names superclass)))
    (if (good-slot-names slot-names superclass-slots)
	;;!!!! BUG: This should have duplicates removed
        (append slot-names superclass-slots)
        (raise-error "Bad slot list -- MAKE-CLASS >> "
                     slot-names
                     superclass-slots))))

;;;slot names must be symbols, and distinct
(define (good-slot-names slots superclass-slots)
  (or (null? slots)
      (and (symbol? (car slots))
           (not (memq (car slots) (cdr slots)))
           (not (memq (car slots) superclass-slots))
           (good-slot-names (cdr slots) superclass-slots))))

;;; New special form (MAKE class slot-names-and-values)
(define (eval-make exp env)
  (let ((class (tool-eval (instance-creation-class exp) env)))
    (if (not (class? class))
        (raise-error "Unrecognized class -- MAKE >> "
                     (instance-creation-class exp))
        (let ((slots (instance-creation-slots exp)))
          (let ((specified-slot-names (map slot-name slots))
                (specified-slot-values
                 (map (lambda (s) (tool-eval (slot-value s) env))
                      slots)))
            (make-standard-instance
             class
             (make-instance-slots
              specified-slot-names
              specified-slot-values
              (class-slot-names class))))))))

;;; for each slot name for the class, initialize it with the value
;;; specified for the instance.  Otherwise initialize it to be
;;; undefined.
(define (make-instance-slots names values all-names)
  (map (lambda (name)
         (get-initial-slot-value name names values))
       all-names))
(define (get-initial-slot-value name names values)
  (cond ((null? names) undefined-value)
        ((eq? name (car names)) (car values))
        (else (get-initial-slot-value name
                                      (cdr names)
                                      (cdr values)))))

;;; Two special functions will be installed in the TOOL environment
;;; for accessing and setting slot values
(define (get-slot object slot-name)
  (if (not (standard-instance? object))
      (raise-error "Unrecognized object -- GET-SLOT >> "
                   object)
      (car (designated-value slot-name object))))
(define (set-slot! object slot-name value)
  (if (not (standard-instance? object))
      (raise-error "Unrecognized object -- SET-SLOT! >> "
                   object)
      (set-car! (designated-value slot-name object)
                value))
  undefined-value)

;;; Given an object and a slot name, return the tail of the list of
;;; slot values beginning with the one with the specified name.
(define (designated-value name object)
  (let ((v
         (named-position name
                         (class-slot-names (instance-class object))
                         (instance-slot-values object))))
    (if v
        v
        (raise-error "Bad slot name for object >> " name v))))

;;; Given a list of names and a corresponding list of values, and
;;; another name, return a pointer to tail of the list of values that
;;; begins with the one with the given name.  This procedure assumes
;;; that the two lists have the name length.
(define (named-position name namelist valuelist)
  (cond ((null? namelist) false)
        ((eq? name (car namelist)) valuelist)
        (else (named-position name
                              (cdr namelist)
                              (cdr valuelist)))))

;;; Data representations for classes, methods, and generic procedures

;;; Classes --- A class has a list of the classes that subsume it and a
;;; list of slot-names

;;; Don't confuse this "make-class" with the MAKE-CLASS special form
;;; in the TOOL language.

;;; Note that the superclass list contains the superclass, the
;;; superclass of the superclass, ....
(define (make-class name superclass slot-names)
  (let ((subsuming
         (if (null? superclass)
             '()
             (cons superclass (class-ancestors superclass)))))
    (list 'class name subsuming slot-names)))
(define (class? exp) (tagged-list? exp 'class))
(define (class-name class) (list-ref class 1))
(define (class-ancestors class) (list-ref class 2))
(define (class-slot-names class) (list-ref class 3))
(define *primitive-class* (make-class '<object> '() '()))

;;; Objects --- An object is a pointer to its class, together with the
;;; values in its slots
(define (make-standard-instance class slot-values)
  (list 'instance class slot-values))
(define (standard-instance? exp) (tagged-list? exp 'instance))
(define (instance-class obj) (list-ref obj 1))
(define (instance-slot-values obj) (list-ref obj 2))

;;; A generic function is a list of methods
(define (make-generic-function name) (list 'generic-function name))
(define (generic-function? exp) (tagged-list? exp 'generic-function))
(define (generic-function-name exp) (list-ref exp 1))
(define (generic-function-methods generic-function)
  (cddr generic-function))
(define (generic-function-set-methods! generic-function methods)
  (set-cdr! (cdr generic-function) methods))
(define (add-method-to-generic-function! method generic-function)
  (let ((current-method
         (find-existing-method
          method
          (generic-function-methods generic-function))))
    (if current-method
        ;; if there already is a method defined for these
        ;; specializers, then replace it with the new one.  Otherwise
        ;; add a new method with the new specializers.
        (method-set-procedure! current-method
                               (method-procedure method))
        (generic-function-set-methods!
         generic-function
         (cons method (generic-function-methods generic-function))))))
(define (find-existing-method method method-list)
  (cond ((null? method-list) false)
        ((same-specializers? method (car method-list)) (car method-list))
        (else (find-existing-method method (cdr method-list)))))

;;; a method is a pair (specializers . procedure) where specializers
;;; is a list of classes and procedure is the corresponding procedure
;;; to apply
(define make-method cons)
(define method-specializers car)
(define method-procedure cdr)
(define (method-set-procedure! method proc) (set-cdr! method proc))

;;check whether two methods have the same specilaizers (e.g. the same
;;list of classes
(define (same-specializers? method1 method2)
  (define (check spec1 spec2)
    (cond ((and (null? spec1) (null? spec2)) true)
          ;; something left over, so number of arugments does not
          ;; match
          ((or (null? spec1) (null? spec2)) false)
          ((eq? (car spec1) (car spec2))
           (check (cdr spec1) (cdr spec2)))
          (else false)))
  (check (method-specializers method1)
         (method-specializers method2)))

;;; extra syntax for TOOL

;;; (DEFINE-GENERIC-FUNCTION)
(define (genfun-definition? exp)
  (tagged-list? exp 'define-generic-function))
(define (generic-function-definition-name exp) (list-ref exp 1))

;;; (DEFINE-METHOD generic-function arglist . body)
(define (method-definition? exp) (tagged-list? exp 'define-method))
(define (method-definition-generic-function exp) (list-ref exp 1))
(define (method-definition-parameters exp) (list-ref exp 2))
(define (method-definition-body exp) (list-tail exp 3))

;;; an argument specified for a method is either a simple name or a
;;; list (name class).  In the first case, the class defaults to
;;; <object>
(define (paramlist-element-name paramlist-element)
  (if (pair? paramlist-element)
      (car paramlist-element)
      paramlist-element))
(define (paramlist-element-class-name paramlist-element)
  (if (pair? paramlist-element)
      (cadr paramlist-element)
      '<object>))

;;; (DEFINE-CLASS name superclass . slots)
(define (class-definition? exp) (tagged-list? exp 'define-class))
(define (class-definition-name exp) (list-ref exp 1))
(define (class-definition-superclass exp) (list-ref exp 2))
(define (class-definition-slot-names exp) (list-tail exp 3))

;;; (MAKE class slot-names-and-values)
(define (instance-creation? exp) (tagged-list? exp 'make))
(define (instance-creation-class exp) (list-ref exp 1))
(define (instance-creation-slots exp) (list-tail exp 2))

;;; slots-and-values are specified as lists (slot-name value)
(define slot-name car)
(define slot-value cadr)

;;; We make some predefined classes that hook to stuff that is
;;; built-in to Scheme.  For example, any number will automatically
;;; belong to the class <number>

;;; Each entry in the following list consists of a class to be
;;; installed in the initial TOOL environment, and a predicate that
;;; tests whether a Scheme object should be a member of that class.
;;; These classes have no slots.  Each one has *primitive-class*
;;; (e.g., the class <object>) as its superclass.
(define scheme-object-classes
  (list
   (list '<boolean> boolean?)
   (list '<number> number?)
   (list '<symbol> symbol?)
   (list '<list> list?)
   (list '<procedure> (lambda (x)
                        (or (compound-procedure? x)
                            (primitive-procedure? x)
                            (generic-function? x))))))

;;; Here is the predicte that tests for a list
(define (list? x) (or (pair? x) (null? x)))

;;; See if an object is in one of the built-in classes.
(define (built-in-class object)
  (define (check-scheme-classes classes)
    (if (null? classes)
        *primitive-class*
        (let ((test-class (car classes)))
          (if ((cadr test-class) object)
              (lookup-variable-value (car test-class)
                                     the-global-environment)
              (check-scheme-classes (cdr classes))))))
  (check-scheme-classes scheme-object-classes))

;;; Primitive procedures are just Scheme procedures and are applied in
;;; the underlying Scheme
(define (primitive-procedure? p) (procedure? p))
(define (apply-primitive-procedure p args) (apply p args))

;;; The following objects will be installed in the initial
;;; environment, with the indicated classes, and bound to the
;;; indicated Scheme objects
(define initial-objects
  (list
   (list 'true true)
   (list 'false false)
   (list 'nil '())))

;;; We need to define the standard FILTER procedure
(define (filter pred l)
  (cond ((null? l) '())
        ((pred (car l)) (cons (car l) (filter pred (cdr l))))
        (else (filter pred (cdr l)))))

;;; This defines the printed representation for various kinds of
;;; objects.
(define (print object)
  '(d ":" (if (pair? object)
              (cons "::" (car object))
              (cons "[]" object)))
  (cond ((null? object)
         (d "NIL"))
        ((standard-instance? object)
         (d "INSTANCE OF" (class-name (instance-class object))))
        ((primitive-procedure? object)
         (d "PRIMITIVE" object))
        ((class? object)
         (d "THE CLASS" (class-name object)))
        ((compound-procedure? object)
         (d "COMBINATION" (cadr object)))
        ((generic-function? object)
         (d "THE GENERIC FUNCTION"
            (generic-function-name object)
            "---"
            (generic-function-methods object)))
        ((or (eq? true object)
             (eq? false object))
         (d "SELF-EVALUATED BOOLEAN" object))
        ((string? object)
         (d "SELF-EVALUATED STRING" object))
        ((number? object)
         (d "SELF-EVALUATED NUMBER" object))
        ((symbol? object)
         (d "SYMBOL" object))
        ((quoted? object)
         (d "QUOTED" object))
        ((pair? object)
         (d "PAIR" object))        
        (else (d "?? OBJECT")))
  '())

(define (d . args)
  "generic display"
  (define (iter a co)
    (if (null? a)
        (co (lambda () (newline)))
        (iter (cdr a)
              (lambda (x)
                (co (lambda ()
                      (display " ")
                      (display (car a))
                      (x)))))))
  (iter args (lambda (x) (x) '__)))

;;; READ EVAL PRINT LOOP
(define eval-error 'nil)
(define (raise-error e . args)
  (print e)
  (map (lambda (x)
         (display " ")
         (print x))
       args
       ;;(~?~)
       )
  (newline)
  (display "stop evaluation.")
  (newline)
  (eval-error 'no-value))
(define (repl)
  (call-with-current-continuation
   (lambda (error)
     (set! eval-error error)
     (newline)
     (display "TOOL==> ")
     (let ((result (tool-eval (read)
                              the-global-environment)))
       ;; note that we call TOOL's PRINT in order to be able to change
       ;; how new classes are printed
       (tool-apply (tool-eval 'print the-global-environment)
                   (list result)))))
  (repl))

;;; The following generic procedures will be initially installed, each
;;; with a method for the specified class
(define initial-generic-procedures
  (list
   (list 'add '(<number> <number>) +)
   (list 'sub '(<number> <number>) -)
   (list 'mul '(<number> <number>) *)
   (list 'div '(<number> <number>) /)
   (list 'eqv '(<number> <number>) eqv?)
   (list 'gt? '(<number> <number>) >)
   (list 'lt? '(<number> <number>) <)
   (list 'sqrt '(<number>) sqrt)
   (list 'cons '(<object> <object>) cons)
   (list 'append '(<list> <list>) append)
   (list 'car '(<list>) car)
   (list 'cdr '(<list>) cdr)
   (list 'null? '(<object>) null?)
   (list 'print '(<object>) print)
   (list 'get-slot '(<object> <symbol>) get-slot)
   (list 'set-slot! '(<object> <symbol> <object>) set-slot!)
   ))
(define initial-procedures
  (list
   (list '+ +)
   (list 'eq? eq?)
   (list 'cons cons)
   (list 'set-car! set-car!)
   (list 'list list)
   (list 'display d)
   (list 'eval tool-eval)
   (list 'apply tool-apply)))

(define the-global-environment '())
(define (make-initial-environment)
  (let ((initial-object-names (map car initial-objects))
        (initial-object-values (map cadr initial-objects))
        (initial-proc-names (map car initial-procedures))
        (initial-proc-values (map cadr initial-procedures)))
    (let ((initial-env
           (extend-environment initial-proc-names
                               initial-proc-values
                               (extend-environment initial-object-names
                                                   initial-object-values
                                                   '()))))
      ;; define the initial class, called <object>
      (define-variable! '<object> *primitive-class* initial-env)
      ;; define the classes that come from Scheme objects
      (for-each
       (lambda (entry)
         (tool-eval
          `(define-class ,(car entry) <object>)
          initial-env))
       scheme-object-classes)
      ;; install initial generic functions and their methods
      (for-each
       (lambda (entry)
         (tool-eval `(define-generic-function ,(car entry))
                    initial-env)
         (let ((gf (tool-eval (car entry) initial-env))
               (specializers
                (map
                 (lambda (c) (lookup-variable-value c initial-env))
                 (cadr entry))))
           (install-method-in-generic-function gf
                                               specializers
                                               (caddr entry))))
       initial-generic-procedures)
      initial-env)))

;;; This is to keep the Scheme printer from going into an infinite
;;; loop if you try to print a circular data structure, such as an
;;; environment
(set! *unparser-list-depth-limit* 10)
(set! *unparser-list-breadth-limit* 10)
(set! *unparser-string-length-limit* 10)
(set! *unparse-with-maximum-readability?* true)

(define restart-tool 'nil)
(define (restart-tool? exp)
  (tagged-list? exp 'restart))
(define (do-restart)
  (display "restarting the evaluator. reset the global environment.")
  (newline)
  (newline)
  (restart-tool 'no-value))
(define (initialize-tool)
  (load "teval")
  (call-with-current-continuation
   (lambda (restart)
     (set! restart-tool restart)
     (set! the-global-environment (make-initial-environment))
     (tool-eval ROOT-OBJECT the-global-environment)
     (tool-eval ASK-METHOD the-global-environment)
     (tool-eval GET-METHOD the-global-environment)
     (repl)))
  (initialize-tool))

;;; EVERYTHING FROM HERE ON IS IDENTICAL TO THE CODE IN CHAPTER 4 OF
;;; THE NOTES, EXCEPT THAT EVAL HAS BEEN RENAMED TO TOOL-EVAL, AND
;;; THERE IS A SMALL CHANGE TO THE SYNTAX OF DEFINE.

;;; Conditionals, sequences, assignments, and definitions are the same
;;; as in Scheme
(define (eval-or-cond clist env)
  (if (no-clauses? clist)
      false
      (or (tool-eval (first-clause clist) env)
          (eval-or-cond (rest-clauses clist) env))))
(define (eval-cond clist env)
  (cond ((no-clauses? clist) false)
        ((else-clause? (first-clause clist))
         (eval-sequence (actions (first-clause clist)) env))
        ((true? (tool-eval (predicate (first-clause clist)) env))
         (eval-sequence (actions (first-clause clist)) env))
        (else (eval-cond (rest-clauses clist) env))))
(define (eval-case key clist env)
  (let ((key-value (tool-eval key env)))
    (cond ((no-clauses? clist) false)
          ((else-clause? (first-clause clist))
           (eval-sequence (actions (first-clause clist)) env))
          ((memq key-value (predicate (first-clause clist)))
           (eval-sequence (actions (first-clause clist)) env))
          (else (eval-case key (rest-clauses clist) env)))))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (tool-eval (first-exp exps) env))
        (else (tool-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
(define (eval-assignment exp env)
  (let ((old-value (tool-eval (assignment-variable exp) env))
        (new-value (tool-eval (assignment-value exp) env)))
    (set-variable-value! (assignment-variable exp) new-value env)
    old-value))
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (tool-eval (definition-value exp) env)
    env)
  undefined-value)
(define undefined-value '*undefined*)

;;; LET syntax
(define (let? exp) (tagged-list? exp 'let))
(define (let->combination exp)
  (define (iter bindings co)
    (cond ((null? bindings)
           (co '() '()))
          (else
           (let ((first (let-first-binding bindings))
                 (rest  (let-rest-binding bindings)))
             (iter rest
                   (lambda (vars expr)
                     (co (cons (let-binding-var first) vars)
                         (cons (let-binding-exp first) expr))))))))
  (iter (let-var-bindings exp)
        (lambda (v e) (let-apply-lambda v (let-body exp) e))))
(define (let-var-bindings exp) (cadr exp))
(define (let-first-binding bindings) (car bindings))
(define (let-rest-binding bindings) (cdr bindings))
(define (let-binding-var binding) (car binding))
(define (let-binding-exp binding) (cadr binding))
(define (let-body exp) (cddr exp))
(define (make-let bindings body) (append '(let) (list bindings) body))
(define (let-apply-lambda vars body bindings)
  (cons (make-lambda-expression vars body) bindings))
(define (make-lambda-expression parameters body)
  (append (list 'lambda parameters) body))
(define (make-lambda-application lambda-expression values)
  (list (cons lambda-expression values)))

;;; procedures are ordinary Scheme procedures fix syntax??
(define (make-procedure lambda-exp env) (list 'procedure lambda-exp env))
(define (compound-procedure? exp) (tagged-list? exp 'procedure))
(define (procedure-text proc) (cadr proc))
(define (parameters proc) (cadr (cadr proc)))
(define (procedure-body proc) (cddr (cadr proc)))
(define (procedure-environment proc) (caddr proc))

;;; Syntax of the language
(define (tagged-list? exp tag) (and (pair? exp) (eq? (car exp) tag)))
(define (self-evaluating? exp) (or (string? exp)
                                   (number? exp)
                                   (eq? exp true)
                                   (eq? exp false)))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (get-environment? exp) (eq? exp 'env))
(define (variable? exp) (symbol? exp))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;;; Definitions in TOOL do not permit the (define (f x) ...)  syntax
;;; of ordinary Scheme, since all procedures are created as methods.
;;; We changed DEFINITION? to enforce this.  We left the underlying
;;; mechanism in DEFINITION-VARIABLE and DEFINITION-VALUE for possible
;;; future use.
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (variable? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (variable? (cadr exp))
      (caddr exp)
      (cons 'lambda
            (cons (cdadr exp)     ;Formal parameters
                  (cddr exp)))))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (conditional? exp) (tagged-list? exp 'cond))
(define (or-conditional? exp) (tagged-list? exp 'or))
(define (case? exp) (tagged-list? exp 'case))

(define (or-clauses exp) (cdr exp))

(define (clauses exp) (cdr exp))
(define (no-clauses? clauses) (null? clauses))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (else-clause? clause) (eq? (predicate clause) 'else))
(define (predicate clause) (car clause))
(define (true? x) (if x true false))
(define (actions clause) (cdr clause))

(define (case-key exp) (cadr exp))
(define (case-clauses exp) (cddr exp))
(define (case-build key clauses) (cons 'case (cons key clauses)))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (application? exp) (pair? exp))
(define (operator app) (car app))
(define (operands app) (cdr app))
(define (no-operands? args) (null? args))
(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))

;;; ENVIRONMENTS
(define (lookup-variable-value var env)
  (let ((b (binding-in-env var env)))
    (if (found-binding? b)
        (binding-value b)
        (raise-error "Unbound variable " var))))
(define (binding-in-env var env)
  (if (no-more-frames? env)
      no-binding
      (let ((b (binding-in-frame var (first-frame env))))
        (if (found-binding? b)
            b
            (binding-in-env var (rest-frames env))))))
(define (pair-up variables values)
  (cond ((null? variables)
         (if (null? values)
             '()
             (raise-error "Too Many A." variables values)))
        ((symbol? variables) (list (cons variables values)))
        ((null? values)
         (raise-error "Too Few A." variables values))
        (else (cons (cons (car variables) (car values))
                    (pair-up (cdr variables)
                             (cdr values))))))
(define (extend-environment variables values base-env)
  (let ((pairs (pair-up variables values)))
    (adjoin-frame (make-frame (map car pairs)
                              (map cdr pairs))
                  base-env)))
(define (set-variable-value! var val env)
  (let ((b (binding-in-env var env)))
    (if (found-binding? b)
        (set-binding-value! b val)
        (raise-error "Unbound variable " var))))
(define (define-variable! var val env)
  (let ((b (binding-in-frame var (first-frame env))))
    (if (found-binding? b)
        (set-binding-value! b val)
        (set-first-frame!
         env
         (adjoin-binding (make-binding var val)
                         (first-frame env))))))

(define (first-frame env) (car env))
(define (rest-frames env) (cdr env))
(define (no-more-frames? env) (null? env))
(define (adjoin-frame frame env) (cons frame env))
(define (set-first-frame! env new-frame) (set-car! env new-frame))

(define (make-frame variables values)
  (cond ((and (null? variables) (null? values)) '())
        ((null? variables)
         (raise-error "Too many values supplied " values))
        ((null? values)
         (raise-error "Too few values supplied "
                      variables))
        (else
         (cons (make-binding (car variables) (car values))
               (make-frame (cdr variables) (cdr values))))))
(define (adjoin-binding binding frame)
  (cons binding frame))
(define (binding-in-frame var frame)
  (cond ((null? frame) no-binding)
        ((eq? var (binding-variable (car frame)))
         (car frame))
        (else (binding-in-frame var (cdr frame)))))

(define (found-binding? b) (not (eq? b no-binding)))
(define no-binding false)
(define (make-binding variable value) (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (set-binding-value! binding value) (set-cdr! binding value))

;;; CLASS-DEF
(define (def-class? exp) (tagged-list? exp 'def-class))
(define (class-definitions exp) (cddr exp))

(define (class-class-variable? exp) (tagged-list? exp 'class-variable))
(define (class-class-variable-name v) (cadr v))
(define (class-class-variable-initial-value v) (caddr v))

(define (class-method? exp) (tagged-list? exp 'method))
(define (class-method-name m) (caadr m))
(define (class-method-params m) (cdadr m))
(define (class-method-body m) (cddr m))

(define (class-slot? exp) (tagged-list? exp 'slot))
(define (class-slot-name-and-value s) (cdr s))

(define (class-parent? exp) (tagged-list? exp 'parent))
(define (class-parent exp) (cadr exp))

(define (class-constructor? exp) (tagged-list? exp 'constructor))
(define (class-constructor-params exp) (cadr exp))
(define (class-constructor-body exp) (cdddr exp))
(define (class-constructor-init-parent-params exp) (cdaddr exp))

(define ASK-METHOD
  '(define (ask obj message . args)
     (let ((m (get-method obj message)))
       (cond (m (apply m args))
             (else (list "Invalid message"
                         message
                         "applied to object ..."))))))
(define GET-METHOD
  '(define (get-method obj message)
     (obj message)))
(define ROOT-OBJECT
  '(define <root>
     (lambda (message)
       (case message
         ((is-a?)
          (lambda (type) (eq? type '<root>)))
         ((type)
          (lambda () '(<root>)))
         ((methods)
          (lambda () '(methods is-a? type)))
         ((new)
          (lambda (self)
            (print "message sent to an instance of the root object")
            (print self)
            '()))
         (else
          (lambda _ "unknown message"))))))

(define TOOL-DEBUG
  (lambda (s x)
    (newline)
    (display "-*-")
    (display s)
    (newline)
    (print x)
    (display "-*-")
    (newline)
    x))

(define class-desugar-define-newclass
  (lambda (class-type
           parent-type
           class-variables
           class-methods
           rest)
    (make-let
     (append `((classtype (quote ,class-type)))
             `((classmethods (quote ,class-methods)))
             `((parent-class (quote ,parent-type)))
             class-variables)
     (list
      '(define get-type (lambda ()
                          (cons classtype
                                (ask parent-class 'type))))
      '(define methods (lambda ()
                         (cons classmethods
                               (ask parent-class 'methods))))
      '(define is-a? (lambda (type)
                       (or (eq? type classtype)
                           (ask parent-class 'is-a? type))))
      rest))))
(define class-desugar-class-dispatcher
  (lambda (rest)
    (list 'lambda '(message . args)
          (case-build 'message
                      `(((type) get-type)
                        ((is-a?) is-a?)
                        ((methods) methods)
                        ((new) ,rest)
                        (else (display "unknown message" message)))))))
(define class-desugar-new-instance-dispatcher
  (lambda (constr-params rest)
    (make-lambda-expression
     constr-params
     (make-lambda-application
      (make-lambda-expression
       (cdr constr-params)
       rest)
      (cdr constr-params)))))
(define class-new-instance-builder-slots-readers-writers
  (lambda (slots rest)
    (define (from-slot-to-object slot)
      (string->symbol
       (string-append
        (symbol->string slot)
        "-reader-writer")))
    (define (from-slot-to-getter slot)
      (string->symbol
       (string-append
        "get-slot-" (symbol->string slot))))
    (define (from-slot-to-setter slot)
      (string->symbol
       (string-append
        "set-slot-" (symbol->string slot) "!")))
    (define make-slot-mutators-accessors-objects
      (lambda (slot init-value)
        (let ((srw (from-slot-to-object slot)))
          (list srw
                (cons '(lambda (v)
                         (lambda (op)
                           (op (lambda () v)
                               (lambda (new) (set! v new)))))
                      init-value)))))
    (define make-slot-getters
      (lambda (slot)
        (let ((srw (from-slot-to-object slot))
              (s->getter (from-slot-to-getter slot)))
          (list s->getter (list srw '(lambda (g _) g))))))
    (define make-slot-setters
      (lambda (slot)
        (let ((srw (from-slot-to-object slot))
              (s->setter (from-slot-to-setter slot)))
          (list s->setter (list srw '(lambda (_ s) s))))))
    (let ((objects (map make-slot-mutators-accessors-objects
                        (map car slots)
                        (map cdr slots)))
          (getters (map make-slot-getters
                        (map car slots)))
          (setters (map make-slot-setters
                        (map car slots))))
      (list (make-let objects
                      (list (make-let getters
                                      (list (make-let setters
                                                      rest)))))))))
(define class-new-instance-builder-init-parent
  (lambda (parent-init-params rest)
    (list (make-let `((parent-instance
                       ,(append '(ask parent-class 'new)
                                parent-init-params)))
                    rest))))
(define class-instance-dispatcher
  (lambda (exp
           methods-name
           methods-params
           methods-defs)
    (let ((cases (map
                  (lambda (n p d)
                    `((,n) ,(cons 'lambda (cons p d))))
                  methods-name
                  methods-params
                  methods-defs)))
      `((lambda (message)
          "INSTANCE DISPATCHER"
          ,(cons 'case
                 (append '(message)
                         cases
                         '((else (get-method parent-instance
                                             message))))))))))

(define class-desugar
  (lambda (exp env)
    (class-desugar-define-newclass
     (class-name exp)
     (glean-parent exp env)
     (class-class-variable-list exp)
     (map class-method-name (class-method-list exp))
     (TOOL-DEBUG "CLASS"
      (class-desugar-class-dispatcher
       (class-desugar-new-instance-dispatcher
        (class-constructor-params
         (glean-constructor exp))
        (class-new-instance-builder-slots-readers-writers
         (class-slot-list exp)
         (class-new-instance-builder-init-parent
          (class-constructor-init-parent-params
           (glean-constructor exp))
          (class-instance-dispatcher
           exp
           (map class-method-name (class-method-list exp))
           (map class-method-params (class-method-list exp))
           (map class-method-body (class-method-list exp))
           )))))))))
(define (eval-def-class exp env)
  (newline)
  ;;; (d "Exp:" exp)
  ;;; (d "Env:" '---)
  (d "parent:" (glean-parent-symbol (class-definitions exp)))
  (d "constructor params:" (class-constructor-params (glean-constructor exp)))
  (d "constructor body:" (class-constructor-body (glean-constructor exp)))
  (d "class variable list:" (class-class-variable-list exp))
  (d "slot list:" (class-slot-list exp))
  (d "method list:")
  (begin (for-each (lambda (m) (d "::"
                                  (class-method-name m)
                                  "=="
                                  (class-method-body m)))
                   (class-method-list exp))
         (newline)
         "--")
  (tool-eval
   `(define ,(class-name exp)
      ,(class-desugar exp env))
   env))

(define (class-name class) (cadr class))
(define (glean-parent-symbol class)
  (call-with-current-continuation
   (lambda (parent)
     (for-each (lambda (x)
                 (and (class-parent? x)
                      (parent (class-parent x))))
               class)
     '<root>)))

(define (glean-parent class env)
  (let ((parent-symbol (glean-parent-symbol class)))
    (tool-eval parent-symbol env)))
(define (glean-constructor class)
  (call-with-current-continuation
   (lambda (constructor)
     (for-each (lambda (x)
                 (and (class-constructor? x)
                      (constructor x)))
               class)
     '(nil))))
(define (class-method-list class)
  (define (iter k)
    (cond ((null? k) '())
          ((and (pair? (car k)) (class-method? (car k)))
           (cons (car k) (iter (cdr k))))
          (else (iter (cdr k)))))
  (iter class))
(define (class-slot-list class)
  (define (iter k)
    (cond ((null? k) '())
          ((and (pair? (car k)) (class-slot? (car k)))
           (cons (let ((s (class-slot-name-and-value (car k))))
                   (if (null? (cdr s))
                       (cons (car s) '('()))
                       s))
                 (iter (cdr k))))
          (else (iter (cdr k)))))
  (iter class))
(define (class-class-variable-list class)
  (define (iter k)
    (cond ((null? k) '())
          ((and (pair? (car k)) (class-class-variable? (car k)))
           (cons (list (class-class-variable-name (car k))
                       (class-class-variable-initial-value (car k)))
                 (iter (cdr k))))
          (else (iter (cdr k)))))
  (iter class))
'load-okay

