(defun evalLi (expr env)
  (ecase (car expr)
	(:LIT (cdr expr))
	(:CAR (cdr expr))
	(:VAR (aref env (cdr expr)))
	(:SETVAR (setf (aref env (cadr expr)) (evalLi (cddr expr) env)))
	(:IF (if (evalLi (second expr) env)
		 (evalLi (third expr) env)
	       (evalLi (cdddr expr) env)))
	(:CALL (apply (second expr) (map-evalLi (cddr expr) env))) 
	(:MCALL (let* ((fun (get-defun (second expr))) 
		       (args (map-mcall-evalLi (cddr expr) env))
		       (nenv (make-array (+ 1 (cadr fun)))))
		  (evalLi (third fun) (make-env-evalLi args env nenv 1)))) ; arrive pas
	(:UNKNOWN (let ((nexpr (lisp2li (second expr) (caddr expr))))
		    (if (eq (car nexpr) :UNKNOWN)
			(error "eval-li ~s" expr)
		      (evalLi (displace expr nexpr) env))))
	(:PROGN (map-progn-evalLi (cdr expr) env))
	(:LET (if (null env) 
		  (let ((nenv (make-array (+ 1 (cadr expr)))))
		    (map-evalLi (cadddr expr) (make-env-let-evalLi (third expr) nenv 1)))
		(let ((nenv (make-array (+ (cadr expr) (array-total-size env)))))
		  (map-evalLi (cadddr  expr) (make-env-let2-evalLi (third expr) env (array-total-size env) nenv 1)))))
	(:COND (map-cond-evalLi (cdr expr) env))
	;(:SETF (warn "setf"))
	(:CASE (map-case-evalLi (evalLi (second expr) env) (cddr expr) env))		    
))

(defun map-evalLi (lexpr env)
  (if (null lexpr)
      nil
    (cons (evalLi (car lexpr) env) 
	  (map-evalLi (cdr lexpr) env))))

(defun make-env-evalLi (args env nenv index)
  (if(null args)
      nenv
    (progn (setf (aref nenv index)  (car args)) 
	   (make-env-evalLi (cdr args) env nenv (+ index 1))))
)

(defun map-mcall-evalLi (lexpr env)
  (if (null lexpr)
      nil
    (if (eq (car lexpr) :LIT)
	(evalLi lexpr env)
      (cons (evalLi (car lexpr) env)
	    (map-evalLi (cdr lexpr) env)))))

(defun displace (cell1 cell2)
  (setf (car cell1) (car cell2) (cdr cell1) (cdr cell2))
  cell1
)

(defun map-progn-evalLi (lexpr env)
   (evalLi (car (last lexpr)) env)
)

(defun make-env-let-evalLi (var nenv index)
  (if (null var)
     nenv
    (progn (setf (aref nenv index) (evalLi (cddar var) nenv)) (make-env-let-evalLi (cdr var) nenv (+ index 1)))))

(defun make-env-let2-evalLi (var env tailleenv nenv index)
  (if (null var)
      nenv
    (if (< index tailleenv)
	(progn (setf (aref nenv index) (aref env index)) (make-env-let2-evalLi var env tailleenv nenv (+ index 1)))
      (progn (setf (aref nenv index) (evalLi (cddar var) env)) (make-env-let2-evalLi (cdr var) env tailleenv nenv (+ index 1))))))

(defun map-case-evalLi (valeur expr env)
  (let ((val (evalLi (car expr) env)))
    (if (eql valeur val)
	(evalLi (cadr expr) env)
      (map-case-evalLi valeur (caddr expr) env))))

(defun map-cond-evalLi (expr env)
  (if (evalLi (car expr) env)
      (evalLi (cadr expr) env)
    (map-cond-evalLi (caddr expr) env)))