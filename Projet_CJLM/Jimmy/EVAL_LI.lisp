;Évalutation du LI

;		Méthodes
;1-make-array
;2-(aref <tab> <i>)
;
;Fonction peuvent être passé en argument
;Apply
;	*(+ 1 2 3 4)
;	*fonction résultat d'un calcul
;	*argument résultat d'un calcul
;	(apply #'+ '(1 2 3 4))
;	(apply #'+ 1 '(1 2 3 4))
;	(apply function argsumentsConnu argumentsInconnu(calculable))
																								;
																								;
																								;
																								;
																								;
																								;																																																																																																																								
(defun eval-LI (expr env)
	(ecase (car expr)
																								;(:LIT . 1)
		(:LIT 
			(cdr expr))
																								;(:VAR )
		(:VAR 
			(aref env (cdr expr)))
																								;(:SETVAR )
		(:SET-VAR 
			(setf (aref env (cadr expr)) 
				(eval-LI (caddr expr) env)))

																								;(:IF)
		(:IF 
			(if (eval-LI (second expr) env) 
				(eval-LI (third expr) env)
				(eval-LI (cadddr expr) env)))
																								;(:CALL + (:LIT . 1)(:LIT . 2))
		(:CALL (apply (second expr)
			(map-eval-li (caddr expr) env)))
																								;(:MCALL FIBO (:LIT . 30))
																								;fun :
																								;args
		(:MCALL 
			(let ((fun (get-defun (second expr)))
			(args (map-eval-li (cddr expr) env)))
																								;(thrid de fun) : corp de la fonction
																								;(second fun) : parametres
																								;(get-defun 'fibo') : 
			(eval-LI (cddr fun)
				(make-env-eval-LI (second fun) args))))
																								;(:PROGN )
		(:PROGN 
			(map-eval-LI-progn PROGN (cdr expr)))
																						;(:UNKNOWN (FIBO (- n 1))) . (n))
		(:UNKNOWN 
			(let (
				(nexpr (lisp2li (seond expr) (cddr expr))))
			(if (eq (car expr) :UNKNOWN)
				(error "eval-LI: ~s" expr)
				(eval-LI (displace expr nexpr) env))))))
																								; expr [ | ]{:UNKNWOWN     |     ((fibo (- n 1)) . (n)))}
																								; (defun displace (cell1 cell2)
																								;			(set-f (car cell2) (car cell1)
																								;				(cdr cell2) (car cell1)
																								;			cell2)			
																								;
																								;
																								;
																								;map-eval-LI
																								;map-env-eval-LI																								
																								;
																								;
																								;
																								;
																								;		JAVASCRIPT
																								;	fermetures
																								;(DEFUN FOO (X Y Z)
																								;	((lambda (u v w)
																								;		)))
																								;
																								;fonction d'ordre supérieur
																								;
																								;fermeture =  	|lambda-function   																							;																																																																																																																	
																								;				| passée comme valeur
																								;				| exécutée ailleur(plustard)
																								;				| capture l'environement courant 	
																								;				|	les variables de l'env lexical englobant
																								;				|	leurs valeurs au moment de la laison entre les deux captures
																								;
																								;		COMPTEUR
																								;	(let ((n 0))
																								;	(defun compteur () n)  --> (set-defun 'compteur'
																								;								(make-closure (lambda() n) {n -> 0}))
																								;	(defun compteur++ ()
																								;		(setf n (+ 1 n)))
																								;	(defun compteur-resert ()
																								;		(setf n 0)))
																								;
																								;	RECURSION ENVELOPPÉE | TERMINALE | TERMINALE PAR CONTINUATION
																								;	(defun length-re(l)
																								;		(if (atom l)
																								;			0
																								;			(+ 1 (length-re (cdr l)))))
																								;	(defun length-rt (l r)
																								;		(if (atom l)
																								;			r
																								;			(length-rt (cdr l) (+ 1 r))))
																								;	(defun length-rc (lc)
																								;		(if (atom l)
																								;			(apply c 0 ())
																								;			(length-rc (cdr l)
																								;				#'(lambda (x)
																								;					(+ 1 (apply c x () ))))
																								;											l1:(1 2 3)		c1:(lambda (x) x) {}
																								;											l2:(2 3)		c2:(lambda (x) (+ 1 ..)) {l1 c1}
																								;											l3:(3)			c3:(lambda (x) (+ 1 ..)) {l2 c2}
																								;											l4:()			c4:(lambda (x) (+ 1 ..)) {l3 c3}
																								;											APPLY
																								;
																								;
																								;
																								;	(defun foo-re (agrs)
																								;		(if (<arret args)
																								;			<init>
																								;			(<env> (foo-re (<next> args))))
																								;
																								;	(defun foo-rc (args c)
																								;		(if (<arret> args)
																								;			(apply c <init> ())
																								;			(foo-rc (<next args)
																								;				#'(lambda (x) 	(<env> (appli c x ())))
																								;								(apply c (<env> x) ()))
																								;
																								;
																								;
																								;
																								;
																								;																																																																																																																								
(defun map-eval-LI (expr env)
	
						;Si c'est un atome 
  (if (atom expr) 
					;On ne fait rien 
      NIL 
					;Sinon on transcrit en LI le premier élément 
					;et on réalise une récursion sur le reste 
    (cons (eval-LI (first expr) env) (map-eval-LI (rest expr) env))))
																								;
																								;
																								;
																								;
																								;
																								;																																																																																																																								
(defun map-eval-LI-progn (expr env)
						;Si c'est un atome 
  (if (atom expr) 
					;On ne fait rien 
	NIL 
					;Sinon on transcrit en LI le premier élément 
					;et on réalise une récursion sur le reste
	(if (atom (rest expr))
		(eval-LI (first expr) env)

		(map-eval-LI-progn (rest expr) env))))
																								;
																								;
																								;
																								;
																								;
																								;																																																																																																																								
(defun make-env-eval-LI (nbArgs listArgs)
  (make_env_rec listArgs 0 (make-array (+ nbArgs 1))))

(defun make_env_rec (listArgs pos envGenerated)
	(when listArgs
		(setf (aref envGenerated pos) (car listArgs))
		(make_env_rec (cdr listArgs) (+ pos 1) envGenerated))
	envGenerated)
																								;
																								;
																								;
																								;
																								;
																								;																																																																																																																								
(defun displace (cell1 cell2)
	(setf (car cell2) (car cell1)
		(cdr cell2) (car cell1))
	cell2)


		;;TEST eval-LI

(load "../02-Generation_Langage_Intermediaire/LISP_to_LI.lisp")

;Make_env
(setf env (make-array 14))
(setf (aref env 0) 'A )
(setf (aref env 1) 'B )
(setf (aref env 2) 'C )
(setf (aref env 3) 'D )
(setf (aref env 4) 'E )
(setf (aref env 5) 'F )
(setf (aref env 6) 'G )
(setf (aref env 7) 'H )
(setf (aref env 8) 'I )
(setf (aref env 9) 'J )
(setf (aref env 10) 'K )
(setf (aref env 11) 'L )
(setf (aref env 12) 'M )
(setf (aref env 13) 'N )

;Trace
(trace eval-LI)
(trace make-env-eval-LI)
(trace second)

;Cas Lit _ Valid
;(eval-LI (LISP2LI 1 env) env)

;Cas Var _ Valid
;(eval-LI (LISP2LI 'g env) env)

;Cas Set-Var _ Valid
;(eval-LI (LISP2LI '(setf g 8) env) env)

;Cas If _ Valid
;(eval-LI (LISP2LI '(IF (EQ 1 2) 3 7) env) env) 

;Cas CALL _ Valid
;(eval-LI (LISP2LI '(EQ 1 1) env) env)

;Cas MCALL 
(set-defun 'looc '(+ x 1))
(eval-LI (LISP2LI '(looc 4) env) env)

(eval-LI '(:MCALL + ((:VAR . 0) (:LIT . 1))) #(3))
;(eval-LI '(:CALL + ((:VAR . 0) (:LIT . 1))) #(3))
;30. Trace: (EVAL-LI '(:CALL + ((:VAR . 0) (:LIT . 1))) '#(3))
;31. Trace: (EVAL-LI '(:VAR . 0) '#(3))
;31. Trace: EVAL-LI ==> 3
;31. Trace: (EVAL-LI '(:LIT . 1) '#(3))
;31. Trace: EVAL-LI ==> 1
;30. Trace: EVAL-LI ==> 4
;4

;Cas PROGN

;Cas UNKNOWN


