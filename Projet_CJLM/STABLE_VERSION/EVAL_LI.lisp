;TEST - FACT:
; (eval-li (lisp2li '(defun fact-j (n)  (if (< n 1)  1  (* n (fact-j (- n 1))))) '()) #())
; (eval-li (lisp2li '(fact-j 3) '()) #())
; (eval-li '(:MCALL FACT-J (:LIT . 3)) #())

;TEST -FIBO:
; (eval-li (lisp2li '(defun fibonae (n) (if (< n 2) n (+ (fibonae (- n 1)) (fibonae (- n 2)))))'() ) ())
; (eval-li (lisp2li '(defun fibonae (n) (if (< n 2) n (+ (fibonae (- n 1)) (fibonae (- n 2)))))'() ) ())																																																																																																																																													
; (eval-li (lisp2li '(fibonae 5) '()) ())


(load "LISP2LI.lisp")

(defun eval-li (expr env)
	;:LIT, :VAR, :SET-VAR, :IF, :CALL, :MCALL, :LET, :PROGN , :UNKNOWN 
	(ecase (car expr)
		
		; (cdr expr) -> 99
		;(eval-li '(:LIT . 99) #()) -> 99																				
		(:LIT (cdr expr))
		
		; (eval-LI '(:VAR . 0) #(A)) -> A																					
		(:VAR
			(aref env 
				(- (cdr expr) 0)
			)
		)
		
		;(eval-li '(:SET-VAR 1 (:LIT . 4)) #(1 2))	-> 4																				
		(:SET-VAR 
			(setf 
				;(second '(:SET-VAR 1 (:LIT . 4))) -> 1 
				(aref env 
					(- (second expr) 0)
				)
				;(third '(:SET-VAR 1 (:LIT . 4))) -> (:LIT . 4) 
				(eval-li (third expr) env )
			)
		)
		
		;(:LET-VAR (setf (aref env (cadr expr)) (eval-li (cddr expr) env )))
		
		;(eval-li '(:IF (:CALL = (:LIT . 1) (:LIT . 2)) (:LIT . 1) (:VAR . 2)) #(1 2 3 4 5) )
		(:IF 
			;(second '(:IF (:CALL = (:LIT . 1) (:LIT . 2)) (:LIT . 1) (:VAR . 2)) )
			(if (eval-li (second expr) env) ; -> (:CALL = (:LIT . 1) (:LIT . 2))
				(eval-li (third expr) env) ;-> (:LIT . 2)
				(eval-li (cadddr expr) env) ; -> (:LIT . 1)
			)
		)
		
		;(eval-li (lisp2li '(+ a b) '(a b)) #(1 2 3) ) -> 3	
		; (:CALL + (:VAR . 1) (:VAR . 2))																			
		(:CALL 
			(apply 
				;(second '(:CALL + (:VAR . 1) (:VAR . 2))) 
				; +
				(second expr) 
				; (cddr '(:CALL + (:VAR . 1) (:VAR . 2)))
				; ((:VAR . 1) (:VAR . 2))
				(map-eval-li (cddr expr) env)
			)
		)
		
		; (eval-li '(:MCALL FACT-J (:LIT . 3)) #())																				
		(:MCALL 
			(let* (
					(fun (get-defun (second expr))) ; (:LAMBDA 1 (:IF (:CALL < (:VAR . 1) (:LIT . 1)) (:LIT . 1) (:CALL * (:VAR . 1) (:MCALL FACT-J (:CALL - (:VAR . 1) (:LIT . 1))))))
				)
				(if (eq (car (cddr expr)) :LIT)
					(let (
							; args = ((:LIT . 3))
							(args (eval-li (cddr expr) env))
						) 
						(eval-li (third fun) ;  (:CALL * (:VAR . 1) (:MCALL FACT-J (:CALL - (:VAR . 1) (:LIT . 1)))))
							(make-env-eval-li 
								args ; args = ((:LIT . 3))
								env ; 
								(make-array (+ 1 (cadr fun))) ;(cadr fun) = 1 ; Nombre d'arguments
								1
							)
						)
					)
	      			(let (
		      				(args 
		      					(cons 
		      						;(car (cddr '(:MCALL FACT-J (:LIT . 3))) 
		      						; (:LIT . 3)
		      						(eval-li (car (cddr expr)) env)
		      						;(cdr (cddr '(:MCALL FACT-J (:LIT . 3))))
		      						; NIL
			    					(map-eval-li (cdr (cddr expr)) env)
			    				)
		      				)
	      				)
						(eval-li (third fun) ;  (:CALL * (:VAR . 1) (:MCALL FACT-J (:CALL - (:VAR . 1) (:LIT . 1)))))
							(make-env-eval-li 
								args ; args = ((:LIT . 3))
								env ;
								(make-array (+ 1 (cadr fun))) ;(cadr fun) = 1 ; Nombre d'arguments
								1 
							)
						)
					)
				)
			)
		)

		;(eval-li (lisp2li '(let ((x 1)) (+ 1 x) ) '(A B)) #(1 2 3 4 5) )
		;(:LET 1 ((:SET-VAR 2 (:LIT . 1))) (:CALL + (:LIT . 1) (:VAR . 3))) -> 6
		(:LET 
			;(let ( (p (cddr expr)) ) 
				;
				(map-eval-li (caddr expr) env) ; ((:SET-VAR 2 (:LIT . 1)))
				(map-eval-li (cdddr expr) env) ; ((:CALL + (:LIT . 1) (:VAR . 3)))
			;)
		)
		
		;(:LAMBDA 
		;	(eval-li (third expr) env)
		;)
			
		;(eval-li '(:PROGN (:CALL + (:LIT . 1) (:LIT . 2)) (:CALL * (:LIT . 1) (:LIT . 6))) #())
		(:PROGN 
			(map-eval-li-progn 
				(PROGN 
					(cdr expr) ; ((:CALL + (:LIT . 1) (:LIT . 2)) (:CALL * (:LIT . 1) (:LIT . 6)))
				) 
				env
			)
		)

		;(:lclosure (:closure env (length env) expr))
		; (eval-li (lisp2li '(myfun 1 2) '(a b c)) #(1 2 3 4 5) )
		; ->  - Error: eval-li (:UNKNOWN (MYFUN 1 2) (A B C))
		(:UNKNOWN 
			(let 
				(
					(nexpr 
						(  lisp2li (second expr) ; (MYFUN 1 2)
							(caddr expr) ; (A B C)
						)
					)
					; nexpr = (lisp2li '(MYFUN 1 2) '(A B C))
					; nexpr = (:UNKNOWN (MYFUN 1 2) (A B C))
				)
			    (if (eq (car nexpr) :UNKNOWN)
					(error "Error: eval-li ~s" expr)
			      	; (displace '(:UNKNOWN (MYFUN 1 2) (A B C)) '(:UNKNOWN (MYFUN 1 2) (A B C)) )
			      	(eval-li (displace expr nexpr) env)
			      	;(:UNKNOWN (MYFUN 1 2) (A B C))

			    )
			)
		)
	)
)	

; expr = ((:VAR . 1) (:VAR . 2)) 
(defun map-eval-li (expr env)
	(if (atom expr)
	    nil
	  	(cons
	  		; (first '((:VAR . 1) (:VAR . 2)))
			(eval-li (first expr) env) ; (:VAR . 1)
			(map-eval-li (rest expr) env) ;(:VAR . 2)
	    )
	)
)
																																																																																																																																																
(defun map-eval-li-progn (expr env)
	(car 
		(last 
			;(car (last '((:CALL + (:LIT . 1) (:LIT . 2)) (:CALL * (:LIT . 1) (:LIT . 6))) ) )
			; -> (:CALL * (:LIT . 1) (:LIT . 6))
			(map-eval-li expr env)
		)
	)
)
																							
;args ; args = ((:LIT . 3))
;env ;
;(make-array (+ 1 1)) -> #(NIL NIL)
;1 
(defun make-env-eval-li (args env nenv index) 
	(if	(null args) ; ((:LIT . 3))
		nenv ;  #(NIL NIL)
		(progn
			(setf (aref nenv index) (car args)) ; 1 = (:LIT . 3)
			(make-env-eval-li (cdr args) env nenv (+ 1 index))
		)
	)
)	
																																																																																																																																																
(defun displace (l ln)
	(RPLACA l (CAR ln))
	(RPLACD l (CDR ln))
    l
)


(defun set-defun (symb lambda)
	(setf 
		(get symb :defun) 
		lambda
	)
)


(defun get-defun (symb)
	(get symb :defun)
)

(trace displace make-env-eval-li map-eval-li-progn map-eval-li eval-li)