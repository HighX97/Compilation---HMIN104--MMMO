;TESTS :
(let x)
(setf x 4)

(LISPTOLI '(defun test_plus (x) (+ x 3)) '(x))
=> (:CALL + ((:VAR 0) (:CONST 3)))

(EVALLI '(:CALL + ((:VAR 0) (:CONST 3))) (make-array 1 :INITIAL-ELEMENT x))
=> 7

(LISPTOLI '(defun test_if (x) (if (< 2 3) 1 0)) '())
=> (:IF (:CALL < ((:CONST 2) (:CONST 3))) (:CONST 1) (:CONST 0))

(EVALLI '(:IF (:CALL > ((:CONST 2) (:CONST 3))) (:CONST 1) (:CONST 0)) '())
=> 0

(EVALLI '(:IF (:CALL < ((:CONST 2) (:CONST 3))) (:CONST 1) (:CONST 0)) '())
=> 1

(EVALLI '(:CALL > ((:VAR 0) (:CONST 3))) (make-array 1 :INITIAL-ELEMENT x))
=> T

(EVALLI '(:CALL < ((:VAR 0) (:CONST 3))) (make-array 1 :INITIAL-ELEMENT x))
=> NIL

(LISPTOLI '(defun test_if_avec_variable (x) (if (< x 3) 1 0)) '(x))
=> (:IF (:CALL < ((:VAR 0) (:CONST 3))) (:CONST 1) (:CONST 0))

(EVALLI '(:IF (:CALL < ((:VAR 0) (:CONST 3))) (:CONST 1) (:CONST 0)) (make-array 1 :INITIAL-ELEMENT x))
=> 0

(EVALLI '(:IF (:CALL > ((:VAR 0) (:CONST 3))) (:CONST 1) (:CONST 0)) (make-array 1 :INITIAL-ELEMENT x))
=> 1

(defun test_plus (x) (+ x 3))
(MEVAL '(test_plus 7))
=> 10

(defun fibonacci (n) (if (<= n 2) 1 (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
(MEVAL '(fibonacci 6))
=> 8

(defun soustractionRecursive (x) (if (= x 0) x (- 1 (soustractionRecursive (- x 1)))))
(MEVAL '(soustractionRecursive 3))
=> 1

(defun factorielle (n) (if (<= n 0) 1 (* n (factorielle (- n 1)))))
(MEVAL '(factorielle 6))
=> 720

(defun test_if (x) (if (< x 3) 1 0))
(MEVAL '(test_if 9))
=> 0
(MEVAL '(test_if 1))
=> 1

(MEVAL '(MEVAL '(fibonacci 6)))
=> 8

(MEVAL '(MEVAL '(MEVAL '(fibonacci 6))))
=> 8

(defun test_loop () (loop for i from 1 to 10 do (print i)))
(MEVAL '(test_loop ))
=> 1 2 3 4 5 6 7 8 9 10 NIL



(defun EVALLI (expr env) 
	(cond 
		;Si c'est une constante
		(
			(eq 
				(car 
					expr
				) 
				:const
			) 
			;On affiche le cdr (la valeur de la constante)
			(car 
				(cdr 
					expr
				)
			)
		)
		;Si c'est une variable
		(
			(eq 
				(car 
					expr
				) 
				:var
			) 
			;On la récupère (sa valeur) dans le tableau des variables d'environnement
			(aref 
				env 
				(car 
					(cdr 
						expr)
				)
			)
		)
		;Si c'est un setf
		(
			(eq 
				(car 
					expr
				) 
				:set-var
			) 
			(setf 
				;On récupère la variable dans le tableau des variables d'environnement
				(aref 
					env 
					(second 
						(cadr 
							expr
						)
					)
				)
				;Et on affecte à cette variable l'évaluation du cddr de l'expression en passant l'environnement courant
				(EVALLI 
					(cddr 
						expr
					) 
					env
				)
			)
		)
		;Si c'est une expression conditionnelle
		(
			(eq 
				(car 
					expr
				) 
				:if
			)
			(if
				;On évalue le test
				(EVALLI 
					(second 
						expr
					) 
					env
				)
				;On évalue l'expression de retour dans le cas positif du test
				(EVALLI 
					(third 
						expr
					) 
					env
				)
				;On évalue l'expression de retour dans le cas négatif du test
				(EVALLI 
					(fourth 
						expr
					) 
					env
				)
			)
		)
		;Si c'est un appel de fonction primitive
		(
			(eq 
				(car 
					expr
				) 
				:call
			)
			;On applique cette fonction primitive à la suite de l'expression
			(apply 
				(cadr 
					expr
				)
				(MAPEVALLI 
					(first 
						(cddr 
							expr
						)
					) 
					env
				)
			)
		)
		;Si c'est une fonction inconnue on teste l'évaluation de la transcription en LI du reste de l'expression
		(
			(eq 
				(car 
					expr
				) 
				:unknown
			)
			(EVALLI 
				(LISPTOLI 
					(second 
						expr
					) 
					(third 
						expr
					)
				) 
				env
			)
		)
		;Si c'est un appel de fonction créée par l'utilisateur
		(
			(eq 
				(car 
					expr
				) 
				:mcall
			) 
			;On affecte à une variable du nom de la fonction le retour de l'évaluation de l'expression
			(let* 
				(
					(fun 
						(GETLIDEFUN 
							(second 
								expr
							)
						)
					)
					(nenv 
						(make-array 
							(+ 
								1 
								(car 
									fun
								)
							)
						)
					)
				)
				(EVALLI 
					(car 
						(cddr 
							fun
						)
					) 
					(MAKEENV 
						(car 
							(cddr 
								expr
							)
						) 
						env 
						nenv 
						1
					)
				)
			)
		)
		;Si c'est un progn d'une expression alors on évalue la suite de l'expression en ne retournant que la valeur du dernier appel récursif
		(
			(eq 
				(car 
					expr
				) 
				:progn)
			(car 
				(last 
					(MAPEVALLI 
						(car 
							(cdr 
								expr
							)
						) 
						env
					)
				)
			)
		)
    )
)

(defun MAPEVALLI (lexpr env)
	(if 
		(null 
			lexpr
		)
		NIL
		(cons
			(EVALLI 
				(first 
					lexpr
				) 
				env
			)
			(MAPEVALLI 
				(rest 
					lexpr
				) 
				env
			)
		)
	)
)


(defun MAKEENV (args env nenv index)
	(if	
		(null 
			args
		)
		nenv
		(progn
			(setf 
				(aref 
					nenv 
					index
				) 
				(EVALLI 
					(car 
						args
					) 
					env
				)
			)
			(MAKEENV 
				(cdr 
					args
				) 
				env 
				nenv 
				(+ 
					1 
					index
				)
			)
		)
	)
)

(defun MEVAL (expr)
	(EVALLI 
		(LISPTOLI 
			expr 
				()
		) 
		()
	)
)

(defun GETLIDEFUN (fun) 
	(get 
		fun 
		:defun
	)
)





;SOUS FORME LINEAIRE

(defun EVALLI (expr env) (cond ((eq (car expr) :const) (car (cdr expr))) ((eq (car expr) :var) (aref env (car (cdr expr)))) ((eq (car expr) :set-var) (setf (aref env (second (cadr expr))) (EVALLI (cddr expr) env))) ((eq (car expr) :if) (if (EVALLI (second expr) env) (EVALLI (third expr) env) (EVALLI (fourth expr) env))) ((eq (car expr) :call) (apply (cadr expr) (MAPEVALLI (first (cddr expr)) env))) ((eq (car expr) :unknown) (EVALLI (LISPTOLI (second expr) (third expr)) env)) ((eq (car expr) :mcall) (let* ((fun (GETLIDEFUN (second expr))) (nenv (make-array (+ 1 (car fun))))) (EVALLI (car (cddr fun)) (MAKEENV (car (cddr expr)) env nenv 1)))) ((eq (car expr) :progn) (car (last (MAPEVALLI (car (cdr expr)) env))))))
(defun MAPEVALLI (lexpr env) (if (null lexpr) NIL (cons (EVALLI (first lexpr) env) (MAPEVALLI (rest lexpr) env))))
(defun MAKEENV (args env nenv index) (if	(null args) nenv (progn (setf (aref nenv index) (EVALLI (car args) env)) (MAKEENV (cdr args) env nenv (+ 1 index)))))
(defun MEVAL (expr) (EVALLI (LISPTOLI expr ()) ()))
(defun GETLIDEFUN (fun) (get fun :defun))

;AVEC WARN

(defun EVALLI (expr env) (cond ((eq (car expr) :const) (warn "1") (car (cdr expr))) ((eq (car expr) :var) (warn "2") (aref env (car (cdr expr)))) ((eq (car expr) :set-var) (warn "3") (setf (aref env (second (cadr expr))) (EVALLI (cddr expr) env))) ((eq (car expr) :if) (warn "4") (if (EVALLI (second expr) env) (EVALLI (third expr) env) (EVALLI (fourth expr) env))) ((eq (car expr) :call) (warn "5") (apply (cadr expr) (MAPEVALLI (first (cddr expr)) env))) ((eq (car expr) :unknown) (warn "6") (EVALLI (LISPTOLI (second expr) (third expr)) env)) ((eq (car expr) :mcall) (warn "7") (let* ((fun (GETLIDEFUN (second expr))) (nenv (make-array (+ 1 (car fun))))) (EVALLI (car (cddr fun)) (MAKEENV (car (cddr expr)) env nenv 1)))) ((eq (car expr) :progn) (warn "8") (car (last (MAPEVALLI (car (cdr expr)) env))))))