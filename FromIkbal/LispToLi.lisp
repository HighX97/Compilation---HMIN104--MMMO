(defun lisp2li (expr env)
  (if (atom expr)
      (if (constantp expr)
	  (cons :LIT expr)
	(if (member expr env)
	    (cons :VAR (+ (position expr env) 1))
	  (cons :CAR expr)))
    (let ((fun (car expr)) (args (cdr expr)))
     (if (consp fun) 
	(if (eq 'lambda (car fun))
	    (error "lambda pas traité ~s" (car fun))
	  (error "une expression evaluable ne commence pas par (( ~s" expr))
      (if (not (symbolp fun))
	  (error "~s n'est pas un symbole" fun) 
	(if (get-defun fun) 
	    (cons :MCALL (cons fun (map-lisp2li args env)))
	  (if (not (fboundp fun))
	      `(:UNKNOWN ,expr ,env)
	    (case fun 
		  (if `(:IF ,(lisp2li (first args) env) 
			    ,(lisp2li (second args) env) 
			    . ,(lisp2li (third args) env))) ;  penser a mettre un car en moins car . 
		  (quote `(:LIT .,(first args))) 
		  (defun 
		    `(:CALL SET-DEFUN (:LIT . ,(first args))
			    (:LIT . (:LAMBDA ,(length (second args)) 
					     ,(lisp2li (third args)  
						       (second  args))))))
		  (let `(:LET ,(length (car args)) 
				, (let-lisp2li (car args) (addtoenv (car args) env))
				, (map-lisp2li (cdr args) (addtoenv (car args) env)))) 
		  (cond (cons :COND (cond-lisp2li args env))) ; forme special 
		  ;(setf (setf-lisp2li args env))
		  (progn (cons :PROGN (map-lisp2li args env)))
		  (case (cons :CASE (case-lisp2li args env)))
		  (t (if (special-form-p fun)  
			 (error "reste forme special ~s" fun) 
		       (if (macro-function fun) 
			   (lisp2li (macroexpand-1 expr) env)
			 (cons :CALL (cons fun (map-lisp2li args env)))
			 )
		  ))))))))))


(defun map-lisp2li (lexpr env)
  (if (atom lexpr)
      nil
    (cons (lisp2li (car lexpr) env)
       (map-lisp2li (cdr lexpr) env))))


(defun get-defun (symb)
  (get symb :defun))


(defun set-defun (symb expr-lambda)
  (setf (get symb :defun)
	expr-lambda))


(defun let-lisp2li (lexpr env)
  (let* ((expr (car lexpr)) (pos (position (car expr) env))) ; let sequentiel pour avoir le nom de la variable
    (if (atom lexpr)
	nil
      (cons `(:SETVAR , pos .,  (lisp2li (cadr expr) env)) (let-lisp2li (cdr lexpr) env))))) 


(defun addtoenv (lexpr env)
  (let ((expr (car lexpr))) ;pour avoir le nom de la variable
    (if (atom lexpr)
	env
      (addtoenv (cdr lexpr) (append env (list (car expr)))))))


;(defun setf-lisp2li (lexpr env)
;  (if (symbolp (car lexpr))
;      (list :SETVAR (lisp2li (car lexpr) env) . (lisp2li (second lexpr) env))
;    (list :SETF (lisp2li (car lexpr) env)))) ; a revoir


(defun cond-lisp2li (lexpr env)
  (let ((expr (car lexpr)))
    (if (atom expr)
	nil
      (list (lisp2li (car expr) env) (lisp2li (second expr) env) (cond-lisp2li (cdr lexpr) env))))) 


(defun case-lisp2li(lexpr env)
  (let ((expr (car lexpr)))
    (if (null expr)
	()
      (if (atom expr)
	  (cons (lisp2li expr env) (case-lisp2li (cdr lexpr) env))
	(list (lisp2li (car expr) env) (lisp2li (second expr) env) (case-lisp2li (cdr lexpr) env))))))
    