(defun LISP2LI (expr env) 
					;Si c'est un atome
  (if 
      (atom expr) 
					;Si c'est une constante 
      (if (constantp expr) 
					;On créé une liste avec :const et la valeur de la constante 
	  (list :const expr) 
					;Sinon (c'est une variable d'environnement) alors on regarde si on connait l'emplacement de la variable 
	(let (pos
	     (position expr env)) 
					;Si la position est connue 
	  (if pos 
					;On créé une liste avec :var et la position de cette variable 
	      (list :var pos) 
					;Sinon c'est une variable inconnue, on créé donc une liste avec :unknown et la variable inconnue 
	    (list :unknown expr))))
					;Si ce n'est pas un atome on récupère le car de l'expr et le cdr de l'expr 
    (let ((fun (car expr)) 
	  (args (cdr expr)))
      (cond 
					;Si c'est une fonction 
       ((eq 'defun fun) 
					;On transcrit le corps de la fonction en LI en lui passant les paramètres et on associe :defun au nom de la fonction 
	(setf (get (first args) :defun) (LISP2LI (third args) (second args)))) 
					;Si c'est une initialisation de variable ou de fonction 
       ((eq 'setf fun) 
					;Si le second élément de l'expression est un symbole 
	(if (symbolp (second expr)) 
					;Alors on créé une liste avec :set-var et on transcrit en LI la seconde partie de l'expression ainsi que la troisième partie de celle-ci 
	    (list :set-var (LISP2LI (second expr) env) (LISP2LI (third expr) env)) 
					;Sinon (c'est une valeur) on créé donc simplement une liste avec :setf et on transcrit en LI la seconde partie de l'expression 
	  (list :setf (LISP2LI (second expr ) env)))) 
					;Si c'est une expression commençant par une quote 
       ((eq 'quote fun) 
					;On fait sauter la quote, on passe donc a une constante contenant l'expression en LISP car on ne sait pas ce qu'il se cache derrière "(first args)" 
	(list :const (first args)))			) 
					;Si c'est une expression conditionnelle 
       ((eq 'if fun) 
					;On concatène :if et on fait un MAPLISP2LI des arguments avec l'environnement pour faire le LI de l'expression et des cas 
	(cons :if (MAPLISP2LI args env))) 
					;Si c'est une expression appellant progn 
       ((eq 'progn fun) 
					;On créé une liste avec :progn et MAPLISP2LI des arguments avec l'environnement 
	(list :progn (MAPLISP2LI args env))) 
					;Si c'est un appel à une fonction déclarée par l'utilisateur 
       ((not (null (get fun :defun))) 
					;On créé une liste avec :mcall, le nom de fonction et la suite qui sera transcrit en LI dans MAPLISP2LI 
	(list :mcall fun (MAPLISP2LI args env))) 
					;Si c'est une fonction inconnue 
       ((not (fboundp fun)) 
					;On créé une liste avec :unknown et la concaténation du nom de fonction et des arguments en LISP ainsi que l'environnement 
	(list :unknown (cons fun args) env)) 
					;Si c'est une fonction prédéfinie 
       ((fboundp fun) 
					;On créé une liste avec :call, le nom de la fonction et la suite qui sera transcrit en LI dans MAPLISP2LI 
	(list :call fun (MAPLISP2LI args env)))))))      

(defun MAPLISP2LI (lexpr env) 
					;Si c'est un atome 
  (if (atom lexpr) 
					;On ne fait rien 
      NIL 
					;Sinon on transcrit en LI le premier élément et on réalise une récursion sur le reste 
    (cons (LISP2LI (first lexpr) env) (MAPLISP2LI (rest lexpr) env))))
