;TESTS :
(LISPTOLI '(defun test_set (x) (setf x 1)) ())
=>(:SET-VAR (:VAR 0) (:CONST 1))

(LISPTOLI '(if (< x 0) 1 0) '(x))
=> (:IF (:CALL < ((:VAR 0) (:CONST 0))) (:CONST 1) (:CONST 0))

(LISPTOLI '(defun factorielle (n) (if (<= n 0) 1 (* n (factorielle (- n 1))))) ())
=> (:IF (:CALL <= ((:VAR 0) (:CONST 0))) (:CONST 1) (:CALL * ((:VAR 0) (:MCALL FACTORIELLE ((:CALL - ((:VAR 0) (:CONST 1))))))))

(LISPTOLI '(defun fibonacci (n) (if (<= n 2) 1 (+ (fibonacci (- n 1)) (fibonacci (- n 2))))) ())
=> (:IF (:CALL <= ((:VAR 0) (:CONST 2))) (:CONST 1) (:CALL + ((:MCALL FIBONACCI ((:CALL - ((:VAR 0) (:CONST 1))))) (:MCALL FIBONACCI ((:CALL - ((:VAR 0) (:CONST 2))))))))

(LISPTOLI '(defun test_quote (x) '(setf x 1)) ())
=> (:CONST (SETF X 1))

(LISPTOLI '(defun test_progn_sur_factorielle (x) (progn (defun factorielle (n) (if (<= n 0) 1 (* n (factorielle (- n 1)))))) ()) ())
=> (:PROGN ((:IF (:CALL <= ((:VAR 0) (:CONST 0))) (:CONST 1) (:CALL * ((:VAR 0) (:UNKNOWN (FACTORIELLE (- N 1)) (N)))))))

(LISPTOLI '(defun test_loop () (loop for i from 1 to 10 do (print i))) ())
=>(:CALL LOOP ((:UNKNOWN FOR) (:UNKNOWN I) (:UNKNOWN FROM) (:CONST 1) (:UNKNOWN TO) (:CONST 10) (:UNKNOWN DO) (:CALL PRINT ((:UNKNOWN I)))))


(defun LISP2LI (expr env) 
	;Si c'est un atome
	(if 
		(atom 
			expr
		) 
		;Si c'est une constante 
		(if 
			(constantp 
				expr
			) 
			;On créé une liste avec :const et la valeur de la constante 
			(list 
				:const 
				expr
			) 
			;Sinon (c'est une variable d'environnement) alors on regarde si on connait l'emplacement de la variable 
			(let 
				(pos 
					(position 
						expr 
						env
					)
				) 
				;Si la position est connue 
				(if pos 
					;On créé une liste avec :var et la position de cette variable 
					(list 
						:var 
						pos
					) 
					;Sinon c'est une variable inconnue, on créé donc une liste avec :unknown et la variable inconnue 
					(list 
						:unknown 
						expr
					)
				)
			)
		)
		;Si ce n'est pas un atome on récupère le car de l'expr et le cdr de l'expr 
		(let 
			(
				(fun 
					(car 
						expr
					)
				) 
				(args 
					(cdr 
						expr
					)
				)
			) 
			(cond 
				;Si c'est une fonction 
				(
					(eq 
						'defun 
						fun
					) 
					;On transcrit le corps de la fonction en LI en lui passant les paramètres et on associe :defun au nom de la fonction 
					(setf 
						(get 
							(first 
								args
							) 
							:defun
						) 
						(LISPTOLI 
							(third 
								args
							) 
							(second 
								args
							)
						)
					)
				) 
				;Si c'est une initialisation de variable ou de fonction 
				(
					(eq 
						'setf 
						fun
					) 
					;Si le second élément de l'expression est un symbole 
					(if 
						(symbolp 
							(second 
								expr
							)
						) 
						;Alors on créé une liste avec :set-var et on transcrit en LI la seconde partie de l'expression ainsi que la troisième partie de celle-ci 
						(list 
							:set-var 
							(LISPTOLI 
								(second 
									expr
								) 
								env
							) 
							(LISPTOLI 
								(third 
									expr
								) 
								env
							)
						) 
						;Sinon (c'est une valeur) on créé donc simplement une liste avec :setf et on transcrit en LI la seconde partie de l'expression 
						(list 
							:setf 
							(LISPTOLI 
								(second 
									expr
								) 
								env
							)
						)
					)
				) 
				;Si c'est une expression commençant par une quote 
				(
					(eq 
						'quote 
						fun
					) 
					;On fait sauter la quote, on passe donc a une constante contenant l'expression en LISP car on ne sait pas ce qu'il se cache derrière "(first args)" 
					(list 
						:const 
						(first 
							args
						)
					)
				) 
				;Si c'est une expression conditionnelle 
				(
					(eq 
						'if 
						fun
					) 
					;On concatène :if et on fait un MAPLISPTOLI des arguments avec l'environnement pour faire le LI de l'expression et des cas 
					(cons 
						:if 
						(MAPLISPTOLI 
							args 
							env
						)
					)
				) 
				;Si c'est une expression appellant progn 
				(
					(eq 
						'progn 
						fun
					) 
					;On créé une liste avec :progn et MAPLISPTOLI des arguments avec l'environnement 
					(list 
						:progn 
						(MAPLISPTOLI 
							args 
							env
						)
					)
				) 
				;Si c'est un appel à une fonction déclarée par l'utilisateur 
				(
					(not 
						(null 
							(get 
								fun 
								:defun
							)
						)
					) 
					;On créé une liste avec :mcall, le nom de fonction et la suite qui sera transcrit en LI dans MAPLISPTOLI 
					(list 
						:mcall 
						fun 
						(MAPLISPTOLI 
							args 
							env
						)
					)
				) 
				;Si c'est une fonction inconnue 
				(
					(not 
						(fboundp 
							fun
						)
					) 
					;On créé une liste avec :unknown et la concaténation du nom de fonction et des arguments en LISP ainsi que l'environnement 
					(list 
						:unknown 
						(cons 
							fun 
							args
						) 
						env
					)
				) 
				;Si c'est une fonction prédéfinie 
				(
					(fboundp 
						fun
					) 
					;On créé une liste avec :call, le nom de la fonction et la suite qui sera transcrit en LI dans MAPLISPTOLI 
					(list 
						:call 
						fun 
						(MAPLISPTOLI 
							args 
							env
						)
					)
				)
			)
		)
	)
)      

(defun MAPLISPTOLI (lexpr env) 
	;Si c'est un atome 
	(if 
		(atom 
			lexpr
		) 
		;On ne fait rien 
		NIL 
		;Sinon on transcrit en LI le premier élément et on réalise une récursion sur le reste 
		(cons 
			(LISPTOLI 
				(first 
					lexpr
				) 
				env
			)
			(MAPLISPTOLI 
				(rest 
					lexpr
				) 
				env
			)
		)
	)
)



;SOUS FORME LINEAIRE

(defun LISPTOLI (expr env) (if (atom expr) (if (constantp expr) (list :const expr) (let ((pos (position expr env))) (if pos (list :var pos) (list :unknown expr)))) (let ((fun (car expr)) (args (cdr expr))) (cond ((eq 'defun fun) (setf (get (first args) :defun) (LISPTOLI (third args) (second args)))) ((eq 'setf fun) (if (symbolp (second expr)) (list :set-var (LISPTOLI (second expr) env) (LISPTOLI (third expr) env)) (list :setf (LISPTOLI (second expr) env)))) ((eq 'quote fun) (list :const (first args))) ((eq 'if fun) (cons :if (MAPLISPTOLI args env))) ((eq 'progn fun) (list :progn (MAPLISPTOLI args env))) ((not (null (get fun :defun))) (list :mcall fun (MAPLISPTOLI args env))) ((not (fboundp fun)) (list :unknown (cons fun args) env)) ((fboundp fun) (list :call fun(MAPLISPTOLI args env)))))))
(defun MAPLISPTOLI (lexpr env) (if (atom lexpr) NIL (cons (LISPTOLI (first lexpr) env) (MAPLISPTOLI (rest lexpr) env))))