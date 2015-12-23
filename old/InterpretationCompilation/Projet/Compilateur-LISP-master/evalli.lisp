;(require "lisp2li.lisp")

;;Tentative de meta evaluateur
;;Gerer les erreurs sur les expressions mal formees (ici ? lisp2li ?)

;(load "C:/Users/Marama/workspace/IN108Project/evalli.lisp")

;(evalli (lisp2li '(defun f (x) (+ 2 x)) ()) ())
;(evalli (lisp2li '(if (< 0 1) 0 1) ()) ())
;(evalli (lisp2li '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1))))) ()) ())
;(evalli (lisp2Li '(fact 4) ()) ())
;(evalli '(:IF ((:CALL <) (:CONST 1) (:CONST 0)) (:CONST 1) (:CONST 0)) '())

;(meval '(defun t15 (n) (if (<= n 0) (+ n n) (* (t15 (- n 1)) n))))
;(meval '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) 
;(meval '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))))
;(meval '(defun fibo2 (n x y) (if (<= n 0) y (fibo2 (- n 1) y (+ x y)))))
;(meval '(defun fiboterm (n) (fibo2 n 1 0)))
;(meval '(defun progtt (x) (progn (+ 1 2) (+ 2 3) (+ x 5))))

(defun meval (expr)
  (evalli (lisp2li expr ()) ()))

(defun evalli2 (expr env)
  (case (car expr)
    (:const (car (cdr expr)))
    (:var (aref env (car (cdr expr))))
    (:set-var (setf (aref env (cadr expr))
                (evalli (cddr expr) env)))
    (:if
      (if
        (evalli (second expr) env)
        (evalli (third expr) env)
        (evalli (fourth expr) env)))
    (:call
      (apply (second expr)
        (mapevalli (first (cddr expr)) env))) ;? liste dans liste
    (:unknown 
      (print :unknown)
      (evalli (lisp2li (second expr) (third expr)) env)) ;gerer l'erreur
    (:mcall
      ;(print (list :mcall (second expr)))
      (let* ((fun (getlidefun (second expr))) ;let sequentiel 
              (nenv (make-array (+ 1 (car fun)))))
              (evalli (car (cddr fun)) (make-fill-env-eval-li (car (cddr expr)) env nenv 1))))
    (:progn 
      (car (last (mapevalli (car (cdr expr)) env)))) ;bricolage :(
    ))

(defun evalli (expr env)
  (cond 
    ((eq (car expr) :const) (car (cdr expr)))
    ((eq (car expr) :var) (aref env (car (cdr expr))))
    ((eq (car expr) :set-var) (setf (aref env (cadr expr))
                (evalli (cddr expr) env)))
    ((eq (car expr) :if)
      (if
        (evalli (second expr) env)
        (evalli (third expr) env)
        (evalli (fourth expr) env)))
    ((eq (car expr) :call)
      (apply (second expr)
        (mapevalli (first (cddr expr)) env))) ;? liste dans liste
    ((eq (car expr) :unknown)
      (evalli (lisp2li (second expr) (third expr)) env)) ;gerer l'erreur
    ((eq (car expr) :mcall)
      (let* ((fun (getlidefun (second expr))) ;let sequentiel 
              (nenv (make-array (+ 1 (car fun)))))
              (evalli (car (cddr fun)) (make-fill-env-eval-li (car (cddr expr)) env nenv 1))))
    ((eq (car expr) :progn)
      (car (last (mapevalli (car (cdr expr)) env)))) ;bricolage :(
    ))

(defun mapevalli (lexpr env)
  (if
    (null lexpr)
    ()
    (cons
      (evalli (first lexpr) env)
      (mapevalli (rest lexpr) env))))


(defun make-fill-env-eval-li (args env nenv index)
  (if	
    (null args)
    nenv
    (progn
      (setf (aref nenv index) (evalli (car args) env))
      (make-fill-env-eval-li (cdr args) env nenv (+ 1 index))
      )))

;(evalli '((:CALL +) (:CONST 1) (:CONST 2)) '())
;(mapevalli '(:IF ((:CALL >) (:CONST 1) (:CONST 0)) (:CONST 1) (:CONST 0)) '())
;(evalli '(:if ((:call >) (:const 1) (:const 0)) (:const 1) (:const 0)) '())
