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
	  (cons :unknown expr))))
					;Si ce n'est pas un atome
					;On stocke (car expr) dans fun 
					;On stocke (cdr expr) dans args
    (let ((fun (car expr)) 
	  (args (cdr expr)))
      (cond
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
	  					;-----Cas let-----
					;Si c'est une initialisation de variable ou de fonction 
       ((eq 'let fun) 
					;Si le second élément de l'expression est un symbole 
	(if (symbolp (second expr)) 
					;Alors on créé une liste avec :set-var et on transcrit en LI 
					;la seconde partie de l'expression 
					;ainsi que la troisième partie de celle-ci 
	    (list :let-var (LISP2LI (second expr) env) (LISP2LI (third expr) env)) 
					;Sinon (c'est une valeur) on créé donc simplement une liste avec 
					;:setf et on transcrit en LI la seconde partie de l'expression 
	  (list :let (LISP2LI (second expr ) env))))
  					;-----Cas Cond----- 		 
					;Si c'est une expression conditionnelle 
       ((eq 'cond fun) 
					;On concatène :if et on fait un MAPLISP2LI des arguments avec 
					;l'environnement pour faire le LI de l'expression et des cas 
	(LISP2LI (macroexpend-1 (third fun)) env))
	;(cons :cond (MAPLISP2LI args env)))	
       				;-----Cas Macro-----
       	((eq 'macro-function fun)
       	;
    (cons :macro-function (LISP2LI (macroexpand-1 expr) env)))	
       				;-----Cas Formspe-----
       	((special-form-p fun)
       	;
    (warn "special-form-p detected"))
					;-----Cas GetDefun-----
       	((eq 'get-defun fun)
       	;
    (warn "get-defun detected"))
					;-----Cas Unknow-----
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

					;--Case
					;--quote,if,defun,let,cond
					;cas macro
					;if (macrofunction fun)
				    	;lisp2li (macroexpend-1 expr)
					;cas formspe
					;if (special-form fun) 
					;warning      







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

		;;TEST LISP2LI

	;LIT
;(LISP2LI 1 '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:LIT . 1)

	;VAR
;(LISP2LI 'a '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:VAR . 0)
;(LISP2LI 'z '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:VAR . 24)

	;QUOTE
;(LISP2LI '(a) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:UNKNOWN (A) (A B C D E F G I J K L M N O P Q R S T U V W X Y Z))

	;IF
;(LISP2LI '(IF (EQ 1 1)) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:IF (:CALL EQ ((:LIT . 1) (:LIT . 1))))

;(LISP2LI '(IF (EQ A B)) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:IF (:CALL EQ ((:VAR . 0) (:VAR . 1))))

	;DEFUN
;(LISP2LI '(defun f (x) (+ 3 x)) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:CALL + ((:LIT . 3) (:VAR . 0)))

	;SETF
;(LISP2LI '(setf (car x) 'x (cadr y) (car x) (cdr x) y) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:SETF (:CALL CAR ((:VAR . 22))))

	;LET
;(LISP2LI '(let ((a 'inside) (b a))
;    (format nil "~S ~S ~S" a b (dummy-function))) '(a b c d e f g i j k l m n o p q r s t u v w x y z))

	;COND
;(LISP2LI '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3))) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(car '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3)) ((eq a 3) (let a 4))))
;COND
;(cdr '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3)) ((eq a 3) (let a 4))))
;(((EQ A 1) (LET A 2)) ((EQ A 2) (LET A 3)) ((EQ A 3) (LET A 4)))
;(cadr '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3)) ((eq a 3) (let a 4))))
;((EQ A 1) (LET A 2))
;(caadr '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3)) ((eq a 3) (let a 4))))
;(EQ A 1)
;(cdadr '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3)) ((eq a 3) (let a 4))))
;((LET A 2))
;(cadadr '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3)) ((eq a 3) (let a 4))))
;(LET A 2)
;(cddr '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3)) ((eq a 3) (let a 4))))
;(((EQ A 2) (LET A 3)) ((EQ A 3) (LET A 4)))
;(caddr '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3))))
;((EQ A 2) (LET A 3))
;(caaddr '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3))))
;(EQ A 2)
;(cdaddr '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3))))
;((LET A 3))
;(cdddr '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3))))
;NIL
;

(defun cond2if (expr)
	(if (atom expr)
		expr
		(if (eq (car expr) 'cond)
			(cond2if (cdr expr))
			(if (atom (car expr))
			expr
			(list 'if (MAPcond2if (car expr)) (MAPcond2if (cdr expr)))))))

(defun MAPcond2if(lexpr) 
					;Si c'est un atome 
  (if (atom lexpr) 
					;On ne fait rien 
      NIL 
					;Sinon on transcrit en LI le premier élément 
					;et on réalise une récursion sur le reste 
    (cons (cond2if (first lexpr)) (MAPcond2if (rest lexpr)))))

;(cond2if '(cond ((eq a 1) (let a 2)) ((eq a 2) (let a 3)) ((eq a 3) (let a 4))))
;(LISP2LI '((IF (EQ A 1) (LET A 2) (IF (EQ A 2) (LET A 3) (IF (EQ A 3) (LET A 4)))))
;(LISP2LI '(if (eq a 1) (let a 2) (if (eq a 2) (let a 3))) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:IF (:CALL EQ ((:VAR . 0) (:LIT . 1))) (:LET-VAR (:VAR . 0) (:LIT . 2))
; (:IF (:CALL EQ ((:VAR . 0) (:LIT . 2))) (:LET-VAR (:VAR . 0) (:LIT . 3))))


	;macro-function
;(LISP2LI '(defmacro macfun (x) '(macro-function 'macfun)) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:CALL DEFMACRO ((:UNKNOWN MACFUN) (:UNKNOWN (X) (A B C D E F G I J K L M N O P Q R S T U V W X Y Z)) (:LIT (MACRO-FUNCTION 'MACFUN))))

	;-----Cas Formspe-----
;(LISP2LI '(defvar a) '(a b c d e f g i j k l m n o p q r s t u v w x y z))

	;-----Cas GetDefun-----

	;-----Cas Unknow-----
;(LISP2LI '(factice 4) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:UNKNOWN (FACTICE 4) (A B C D E F G I J K L M N O P Q R S T U V W X Y Z))

	;-----Cas Call----- 
;(LISP2LI '(member 2 '(1 2 3)) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:CALL MEMBER ((:LIT . 2) (:LIT (1 2 3))))

;(LISP2LI '(facto 4) '(a b c d e f g i j k l m n o p q r s t u v w x y z))
;(:CALL FACTO ((:LIT . 4)))
