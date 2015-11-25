;;;; Created on 2012-11-23 10:27:35

(require "lisp2li.lisp")
;(comp '(defun fibo (n) (if (<= n 0) 1 (+ (fibo (- n 1)) (fibo (- n 2))))))

;(load "C:/Users/Marama/workspace/IN108Project/litoasm.lisp")
;(load "/auto_home/dvaret/workspace/LispProjet/litoasm.lisp")

;li -> langage vm
;(litoasm '(:CALL + (:CONST 1) (:CONST 2)) ())

;(1 SOM ((:CALL +) (:VAR 0) (:CONST 1)))
;(comp '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1))))))
;(comp '(cond ((< 0 1) 0) ((> 0 1) 1)))

;(comp '(defun t0 (n) (if (<= n 0) (+ n n) (* (t0 (- n 1)) n))))
;(comp '(defun fibof (n x y) (if (<= n 0) y (fibof (- n 1) y (+ x y)))))
;(lisp2li '(defun fibo20 (n x y) (if (<= n 0) y (fibo20 (- n 1) y (+ x y)))) ())

;(lisp2li '(defun t20 (n) (if (<= n 0) (+ n n) (* (t20 (- n 1)) n))) ())
;(comp '(defun  fct (n) (progn (+ 1 2) (+ 2 3) (+ 3 4) (+ 4 n))))

(defun comp (expr)
 (print (lisp2li expr ()))
  ;(print :)
  (litoasm (lisp2li expr ()) ()))

(defun litoasm (expr env)
  (if (numberp (first expr))
    (let ((name (second expr)) (args (third expr)))
      (append
        (list (list :label name) (list :stack (first expr)))
        (litoasm args env)
        (list (list :rtn))))
    (case (first expr)
      (:if
        (append
          (litoasm (second expr) env)
          (list (list :skipnil (+ 1 (length (litoasm (copy-list (third expr)) env))))) ; + 1 ?
          (litoasm (third expr) env)
          (list (list :rtn))
          (litoasm (fourth expr) env)
          ))
      (:call
        ;(print expr)
        (let ((ll (length (third expr))))
          (if(= ll 0)
          (list (list :call (second expr)))
          (if (= ll 1)
            (append
              (maplitoasm (third expr) env)
              (list (list :call (second expr))))
            (append
              (maplitoasm (third expr) env)
              (calltoasm (second expr) ll))))))
      (:mcall
        (append
          (maplitoasm (third expr) env)
          (list (list :call (second expr)))))
      (:const (list (list :const (second expr))))
      (:var (list (list :var (second expr))))
      (:progn 
        ;(print (first (rest (first (rest expr)))))
        (maplitoasm (first (rest expr)) env))
      ;(vmeval 'vm '(defun loop_test (li n) (loop for cel in li do (atom cel))))
      ;(comp '(defun loop_test (li n) (loop for cel in li do (atom cel))))
      ;(lisp2li '(defun loop_test (li n) (loop for cel in li do (atom cel))) ())
      (:loop
        (append
          ;(print env)
          (list (list :label (third expr)))
          (litoasm (second expr) env)
          (list (list :call 'car))
          (list (list :call 'atom))
          (list (list :skiptrue (+ 1 (length (litoasm (copy-list (fourth expr)) env)))))
          (litoasm (fourth expr) env)
          (litoasm (fifth expr) env)
          (list (list :jump (third expr)))
          ))
      (:setf ())
      (:set-var 
        (append
          (litoasm (third expr) env)
          (list (list :set-var (second (second expr))))
            ))
      (:let-var 
        (append
          ;(list (list :const (third expr)))
          (litoasm (third expr) env)
          (list (list :store))
          ))
      ;(lisp2li '(defun aaa (x y z) (let ((a 5) (b 10)) (+ a x) (+ y b))) ())
      ;(comp '(defun aaa (x y z) (let ((a 5) (b 10)) (+ a x) (+ y b))))
      ;(vmeval 'vm '(defun aaa (x y z) (let ((a 5) (b 10)) (+ a x y b))))
      (:let 
        ;(print env)
        (append
          (maplitoasm (third expr) ())
          ;(list (list :stack (second expr)))
          (maplitoasm (fourth expr) ())
          ))
      (:unknown 
        ;erreur ?
        (let ((expr2 (lisp2li (second expr) (third expr))))          
          (litoasm expr2 env)))
      )))


(defun calltoasm (expr n)
  ;(print expr)
  (if
    (<= n 1)
    ()
    (cons (list :call expr) (calltoasm expr (- n 1))))
    )

(defun maplitoasm (lcode env)
  (if (atom lcode)
      ()
      (append
       (litoasm (first lcode) env)
       (maplitoasm (cdr lcode) env))))
