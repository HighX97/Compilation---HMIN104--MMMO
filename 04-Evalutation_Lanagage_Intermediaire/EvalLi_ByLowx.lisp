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
		(:SETVAR 
			(setf (aref env (cdr expr)) 
				(eval-LI (cddr expr) env)))
																								;(:IF)
		(:IF 
			(if (eval-LI (second expr) env) 
				(eval-LI (thrd expr) env)
				(eval-LI (cdddr expr) env)))
																								;(:CALL + (:LIT . 1)(:LIT . 2))
		(:CALL (apply (second expr)
			(map-eval-li (cddr expr) env)))
																								;(:MCALL FIBO (:LIT . 30))
																								;fun :
																								;args
		(:MCALL 
			(let ((fun (get-defun (second expr))))
			(args (map-eval-li (cddr expr))))
																								;(thrid de fun) : corp de la fonction
																								;(second fun) : parametres
																								;(get-defun 'fibo') : 
			(eval-LI (thrid de fun)
				(make-env-eval-LI (second fun) args)))
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
(defun make-env-eval-LI (expr env)
						;Si c'est un atome 
  (if (atom expr) 
					;On ne fait rien 
      NIL 
					;Sinon on transcrit en LI le premier élément 
					;et on réalise une récursion sur le reste 
    (make-array (make-env-eval-LI (cdr expr )))))
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
