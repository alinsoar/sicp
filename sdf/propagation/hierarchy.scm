
;;;; Hierarchical organization of objects

(define (make-relations name parent)
  (let ((children '()))

    (define (get-name)
      name)

    (define (set-name! new-name)
      (set! name new-name))

    (define (get-parent)
      parent)

    (define (get-children)
      children)

    (define (add-child! child)
      (set! children (lset-adjoin eq? children child)))

    (define (clear-children!)
      (set! children '()))

    (bundle relations?
            get-name
            set-name!
            get-parent
            get-children
            add-child!
            clear-children!)))

(define relations?
  (make-bundle-predicate 'relations))

;; Implies a get-relations operation returning a relations.
(define (relatable? object)
  (and (bundle? object)
       (predicate<= (bundle-predicate object)
                    relatable?)))
(register-predicate! relatable? 'relatable)

(define (get-name relatable)
  (guarantee relatable? relatable 'get-name)
  ((relatable 'get-relations) 'get-name))

(define (get-parent relatable)
  (guarantee relatable? relatable 'get-parent)
  ((relatable 'get-relations) 'get-parent))

(define (get-children relatable)
  (guarantee relatable? relatable 'get-children)
  ((relatable 'get-relations) 'get-children))

(define (add-child! child relatable)
  (guarantee relatable? child 'add-child!)
  (guarantee relatable? relatable 'add-child!)
  ((relatable 'get-relations) 'add-child! child))

(define top-level-parent?
  (make-bundle-predicate 'top-level-parent))
(set-predicate<=! top-level-parent? relatable?)

(define universal-ancestor
  (let ((relations (make-relations 'top-level-parent #f)))

    (define (get-relations)
      relations)

    (define (clear!)
      (relations 'clear-children!))

    (bundle top-level-parent? get-relations clear!)))

(define *my-parent*
  (make-parameter universal-ancestor))

(define (clear-relatable-hierarchy!)
  (universal-ancestor 'clear!))

(define (path-of object)
  (cond ((symbol? object) object)
        ((relatable? object)
         ;; TODO: nothing handles name conflicts for a given
         ;; container.
         (map get-name (ancestry object)))
        (else
         (error "Unsupported input:" object 'path-of))))

(define (ancestry relatable)
  (guarantee relatable? relatable 'ancestry)
  (let ((parent (get-parent relatable)))
    (if parent
        (cons relatable (ancestry parent))
        '())))

(define (path->thing path)
  (let loop
      ((path (reverse path))
       (relatable universal-ancestor))
    (if (pair? path)
        (let ((relatable*
               (find (lambda (child)
                       (equal? (get-name child) (car path)))
                     (get-children relatable))))
          (if (not relatable*)
              (error "Unknown name:" (car path)))
          (loop (cdr path) relatable*))
        relatable)))
