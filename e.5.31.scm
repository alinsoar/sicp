
(load "aux/sicp")
(load "aux/compiler")

(define test
  (lambda (expr)
    (for-each (lambda _ (display " ~ ")) (iota 20))
    (newline)
    (__d "expr............" expr)
    (let ((compiled-code (compile expr 'val 'next)))
      (__d "reg needed......" (car compiled-code))
      (__d "reg modified...." (cadr compiled-code))
      (newline)
      (map
        (lambda (x) (if (symbol? x)
                 (__d x)
                 (__d "    " x)))
        (caddr compiled-code)))))

(for-each test
  '(
     ;;  no register is saved in the first 2 cases
     (f 'x 'y)
     ((f) 'x 'y)
     ;; in each of these 2 cases are saved the PROC and ARGL registers
     (f (g 'x) y)
     (f (g 'x) 'y)

     ;; other cases
     ;; ((f) x y)                        ; ENV is saved
     ;; (f x y)                          ; none
     ;; (f (x) y)                        ; PROC and ARGL are saved
     ;; (f 'x y)                         ; none
     ;; (f x 'y)                         ; none
     ;; (f x (y))                        ; PROC and ENV are saved
     ))
