;LISP TO LI
(defun LISP2LI (expr env) 
					;Si c'est un atome
  (if 
      (atom expr) 
					;-----Cas literal-----
      (if (constantp expr) 
					;On créé une liste avec :lit et la valeur de la constante 
	  (cons :lit expr) 
					;-----Cas variable-----
					;On stocke dans pos la position de la variable dans l'env
	(let ((pos
	     (position expr env))) 
					;Position connue 
	(if pos 
					;On créé une liste avec :var et pos 
	    (cons :var pos) 
					;Position inconnue
					;On créé une liste avec :unknown et la variable expr 
	  (list :unknown expr))))
					;Si ce n'est pas un atome
					;On stocke (car expr) dans fun 
					;On stocke (cdr expr) dans args
    (let ((fun (car expr)) 
	  (args (cdr expr)))
      (cond 
					;-----Cas Defun-----
       ((eq 'defun fun) 
					;On transcrit le corps de la fonction en LI en lui passant 
					;les paramètres et on associe :defun au nom de la fonction
	(set-defun (first args) (LISP2LI (third args) (second args))))
	;(setf (get (first args) :defun) (LISP2LI (third args) (second args)))) 
					;-----Cas setf-----
					;Si c'est une initialisation de variable ou de fonction 
       ((eq 'setf fun) 
					;Si le second élément de l'expression est un symbole 
	(if (symbolp (second expr)) 
					;Alors on créé une liste avec :set-var et on transcrit en LI 
					;la seconde partie de l'expression 
					;ainsi que la troisième partie de celle-ci 
	    (list :set-var (LISP2LI (second expr) env) (LISP2LI (third expr) env)) 
					;Sinon (c'est une valeur) on créé donc simplement une liste avec 
					;:setf et on transcrit en LI la seconde partie de l'expression 
	  (list :setf (LISP2LI (second expr ) env))))
					;-----Cas Quote----- 
					;Si c'est une expression commençant par une quote 
       ((eq 'quote fun) 
					;On fait sauter la quote, on passe donc a un literal 
					;contenant l'expression en LISP  
	(list :lit (first args)))	
					;-----Cas If----- 		 
					;Si c'est une expression conditionnelle 
       ((eq 'if fun) 
					;On concatène :if et on fait un MAPLISP2LI des arguments avec 
					;l'environnement pour faire le LI de l'expression et des cas 
	(cons :if (MAPLISP2LI args env))) 
					;-----Cas GetDefun-----
					;-----Cas Unknow-----
					;--Case
					;--quote,if,defun,let,cond
					;cas macro
					;if (macrofunction fun)
				    	;lisp2li (macroexpend-1 expr)
					;cas formspe
					;if (special-form fun) 
					;warning
					;Si c'est une fonction inconnue 
       ((not (fboundp fun)) 
					;On créé une liste avec :unknown et la concaténation du nom de 
					;fonction et des arguments en LISP ainsi que l'environnement 
	(list :unknown (cons fun args) env))
					;;-----Cas Call----- 
					;Si c'est une fonction prédéfinie 
       ((fboundp fun) 
					;On créé une liste avec :call, le nom de la fonction et la suite 
					;qui sera transcrit en LI dans MAPLISP2LI 
	(list :call fun (MAPLISP2LI args env)))))))      







;MAPLISP TO LI
(defun MAPLISP2LI (lexpr env) 
					;Si c'est un atome 
  (if (atom lexpr) 
					;On ne fait rien 
      NIL 
					;Sinon on transcrit en LI le premier élément 
					;et on réalise une récursion sur le reste 
    (cons (LISP2LI (first lexpr) env) (MAPLISP2LI (rest lexpr) env))))

;;Get-Defun
(defun get-defun (symb)
  (get symb :defun))

;;Set-Defun
;;symb : expression evaluable mais pas evaluee
;;expr-lambda : expression evaluable evaluee
(defun set-defun (symb expr-lambda)
  (setf (get symb :defun)
	expr-lambda))
