																																																																																																																																																
(defun eval-LI (expr env)
	(ecase (car expr)
																							
		(:LIT (cdr expr))
																								
		(:VAR (aref env (cdr expr)))
																							
		(:SET-VAR 
			(setf (aref env (cadr expr)) 
				(eval-LI (cddr expr) env )
			)
		)
		
		(:LET-VAR (setf (aref env (cadr expr)) (eval-LI (cddr expr) env )))
																								
		(:IF (if (eval-LI (second expr) env) 
			     (eval-LI (third expr) env)
			     (eval-LI (cdddr expr) env)))
																							
		(:CALL (apply (second expr)
			(map-eval-li (cddr expr) env)))
																								
																								
																								
		(:MCALL (let* ((fun (get-defun (second expr))))
			(if (eq (car (cddr expr)) :LIT)
				(let ((args (eval-LI (cddr expr) env))) 
					(eval-LI (third fun) (make-env-eval-li args env (make-array (+ 1 (cadr fun))) 1)))
      			(let ((args (cons (eval-LI (car (cddr expr)) env)
	    				(map-eval-LI (cdr (cddr expr)) env))))
							(eval-LI (third fun)
				(make-env-eval-li args env (make-array (+ 1 (cadr fun))) 1))))))
			;(eval-LI (cddr fun)
			;	(make-env-eval-LI (second fun) args))))
		
		;(:MCALL (let* ((fun (get-defun (second expr))) 
		 ;      (args (map-eval-Li (cddr expr) env))
		  ;     (nenv (make-array (+ 1 (cadr fun)))))
		 ; (evalLi (third fun) (make-env-eval-li args env nenv 1)))) se
		(:LET 
			;(let ( (p (cddr expr)) ) 
				(map-eval-li (caddr expr) env)
				(map-eval-li (cdddr expr) env)
			;)
		)
		
		(:LAMBDA 
			
			(eval-li (third expr) env))
			
				
		(:PROGN (map-eval-LI-progn (PROGN (cdr expr)) env))

		;(:lclosure (:closure env (length env) expr))

		(:UNKNOWN (let ((nexpr (lisp2li (second expr) (caddr expr))))
		    (if (eq (car nexpr) :UNKNOWN)
			(error "eval-li ~s" expr)
		      (eval-LI (displace expr nexpr) env))))
																						
		
		))																		
																																																																																																																																																
(defun map-eval-LI (expr env)
	
  (IF (atom expr)
       nil
  	    (cons
          (eval-LI (first expr) env)
          (map-eval-LI (rest expr) env))))
																																																																																																																																																
(defun map-eval-LI-progn (expr env)
	(car (last (map-eval-LI expr env))))
																								
(defun make-env-eval-li (args env nenv index) 
  (if	
    (null args)
    nenv
    (progn
      (setf (aref nenv index) (car args))
      (make-env-eval-li (cdr args) env nenv (+ 1 index))
      )))	
 
    																						
																																																																																																																																																
(defun displace (l ln)
	(RPLACA l (CAR ln))
   (RPLACD l (CDR ln))
     l) 
(defun make-env-eval-LIA (nbArgs listArgs)
  (make_env_rec listArgs 0 (make-array (+ nbArgs 1))))

(defun make_env_rec (listArgs pos envGenerated) 
	(when listArgs
		(setf (aref envGenerated pos) (car listArgs))
		(make_env_rec (cdr listArgs) (+ pos 1) envGenerated))
	envGenerated)	

	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	(defun LISP2LI (expr env) 
	 (if (atom expr) 
		
		(if	(constantp expr) 
			
			(cons :lit expr) 
			(let ((pos (position expr env))) 
				
				(if pos 
					
					(cons :var (+ 1 pos)) 
					(warn "~s est non existante dans l'environnement" expr)
				)
			)
		)
		
		(let ((fun (car expr)) 
				(args (cdr expr))) 
			(cond
				
    			((get-defun fun) 
					
					(list* :mcall fun (MAPLISP2LI args env))
				) 
				 
				((not (fboundp fun)) 
					
					(list :unknown 

						(cons fun args) env)
				) 
				((eq 'defun fun)
					;(list*
					;	:call 'set-defun (cons :lit (first args)) 
					;	(list* :lit :lambda (length (second args))
					;	 (LISP2LI (third args) (second args))))) 
					(set-defun (first args) 
						(list :call 'set-defun (cons :lit (first args))
							 (list* :lit :lambda (length (second args))
								(list (LISP2LI (third args) (second args)))))) )
				
				((eq 'let fun) 
					(list* :let (length (first args))
					
					 	;(let* ((mam (car (car args))))
					 	;	(if (symbolp (first mam)) 
	    				;		(list* :let-var (cdr (LISP2LI (first mam) (append env (list (first mam))))) 
	    				;			(LISP2LI (cadr mam) env)
	    				;			(LISP2LI (rest args) (append env (list (first mam) (car (rest args)))))) 
	  					;			(MAPLISP2LI (rest args) env)))))
						;(l2liLet (car args) (append env (list (caaar args)))) (append (car args) env)
            			(l2liLet (car args) (newenv (car args) env))
            			;(LISP2LI (rest args) (append env (list (caaar args) (caadar args))))
            			(MAPLISP2LI (rest args) (newenv (car args) env))))
 
				((eq 'setf fun) 
					(let ((pos (position (first args) env))) 
				          	(if pos 
					           (if (symbolp (second expr)) 
						           (list :set-var (cdr (LISP2LI (second expr) env)) 
							       (LISP2LI (third expr) env)
						                 ) 
						
						(cons :setf (LISP2LI (second expr) env)
						)
					)
					(warn "~s est non existante dans l'environnement" (first args))          
				
				          	))) 
				
				((eq 'quote fun) 
					
					(cons :const (first args)
					)
				) 
				 
				((eq 'if fun)
					;(cons :if (MAPLISP2LI args env)
					(list :if (LISP2LI (first args) env)
					 		  (LISP2LI (second args) env)
							  (LISP2LI (third args) env)
					)
				) 
				 
				((eq 'progn fun) 
					 
					(cons :progn (MAPLISP2LI args env)
					)
				)

				((eq 'cond fun) 
						;On concatène :if et on fait un MAPLISP2LI des arguments avec 
						;l'environnement pour faire le LI de l'expression
					;((rplace fun 'if) (LISP2LI expr env)))
					(LISP2LI (macroexpand-1 expr) env))
						;(cons :cond (MAPLISP2LI args env)))
				((eq 'case fun) 
						
					(LISP2LI (macroexpand-1 expr) env))
				
						;-----cas Loop-----
					
				((eq 'loop fun) 
					(if (eq (first args) 'while)
						(list* :While 
							(if (atom (second args)) (second args) (LISP2LI  (second args) env))
									(MAPLISP2LI (list (fourth args)) env)
						)
						(warn "~s Loop non traite " (first args))
					)
				)
       					;-----Cas Macro-----
				
				 ((eq 'macro-function fun)
       				;
    				(cons :macro-function (LISP2LI (macroexpand-1 expr) env))) 

				((fboundp fun) 
					 
					(list* :call fun (MAPLISP2LI args env))
				)
			)
		)
	)
)    


(defun set-defun (symb lambda)
  (setf (get symb :defun) lambda))

(defun get-defun (symb)
  (get symb :defun))

(defun MAPLISP2LI (lexpr env) 
	(if (atom lexpr)  
		NIL 
		(cons (LISP2LI (first lexpr) env)
			(MAPLISP2LI (rest lexpr) env)
		)
	)
)
(lisp2li '(defun mfi (l) (if (atom l) 0 (+ 1 (mf (rest l))))) nil)
(defun l2liLet(expr env)
  (let* ((mam (car expr)))
  (if (atom expr)
    ()
      (cons (list :set-var (position (car mam) env)
        (lisp2li (cadr mam) env)) (l2liLet (cdr expr) env)
        ))))

(defun newenv (expre env)
          (if (atom expre)
            env
            (newenv (cdr expre) (append env (list (caar expre))))))

(defun fibo(n) (if (<= n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))
(defun factorielle(n) (if (<= n 0) 1 (* n (factorielle (- 1 n)))))
(defun facto(n) (if (= n 0) 1 2)) 
(trace displace make-env-eval-li map-eval-LI-progn map-eval-LI eval-LI)
(trace newenv l2lilet MAPLISP2LI get-defun set-defun LISP2LI)-