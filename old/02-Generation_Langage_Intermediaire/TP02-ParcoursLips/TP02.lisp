;; CAS DEFUN
;; (:LAMBDA ,(length (second args)) .,(lisp2li (third args) (second args)))

;;Parcours-Lisp1
(defun parcours-lisp1 (exp env)
  (if (atom exp)
      (if (constantp exp)
	  1
	(if (member exp env :test 'eql :key 'car)
	    1
	  (warn "You're using an undefine variable in the environement")))	
					; ,x --> valeur(x)
    (let ((fun (car exp))
	  (args (cdr exp)))
      (if (not (symbolp fun))
	  (if (and (consp fun) (eq 'lambda (car fun)))
	      ;;Cas LambdaExpression
	      (map-parcours-lisp1 args env)
	    (warn "Exp peut être une fonction"))
	;;Cas Defun
	(if (eq 0 0)
	    0
	  0
	  )
	;;Cas MCall
	(if (eq 1 1 )
	    0
	  0)
	;;Cas quote
	(if (eq 2 2)
	    0
	  0)
	;;Cas if
	(if (eq 3 3)
	    0
	  0)
	;;Cas let
	(if (eq 4 4)
	    0
	  0)
	;;Cas Call
	(if (eq 5 5)
	    0
	  0)))))

;;Parcours-Lisp2
(defun parcours-lisp2 (exp env)
  (if (atom exp)
      (if (constantp exp)
	  1
	(if (member exp env :test 'eql :key 'car)
	    1
	  (warn "You're using an undefine variable in the environement")))	
					; ,x --> valeur(x)
    (let ((fun (car exp))
	  (args (cdr exp)))
      (if (not (symbolp fun))
	  (if (and (consp fun) (eq 'lambda (car fun)))
	      ;;Cas LambdaExpression
	      (map-parcours-lisp1 args env)
	    (warn "Exp peut être une fonction"))
	;;Cas Defun
	(if (eq 0 0)
	    0
	  0
	  )
	;;Cas MCall
	(if (eq 1 1 )
	    0
	  0)
	;;Cas quote
	(if (eq 2 2)
	    0
	  0)
	;;Cas if
	(if (eq 3 3)
	    0
	  0)
	;;Cas let
	(if (eq 4 4)
	    0
	  0)
	;;Cas Call
	(if (eq 5 5)
	    0
	  0)
	;))))

;;Lisp2Li

(defun lisp2li (expr env)
					;env = liste des variables
					;expr = expression LISP evaluable  (if (atom exp)
  (if (constantp exp)
      1
    (if (member exp env :test 'eql :key 'car)
	1
      (warn "You're using an undefine variable in the environement")))	
					; ,x --> valeur(x)
  (let ((fun (car exp))
	(args (cdr exp)))
    (if (not (symbolp fun))
	(if (and (consp fun) (eq 'lambda (car fun)))
	    ;~~~~~~~~~Cas LambdaExpression~~~~~~~~
	    (map-parcours-lisp1 args env)
	  (warn "Exp peut être une fonction"))
      ;~~~~~~~~Cas Defun~~~~~~~~
      (if (eq 0 0)
	  0
	0
	)
      ;~~~~~~~~Cas MCall~~~~~~~~
      ;fboundp is true if the symbol has a global function definition.
      (if (not (fboundp fun))
	  ;;~~~~~~~~Cas Unknow~~~~~~~~
	  (if (eq 4 4)
	      0
	    0)
	0)
      ;;~~~~~~~~Cas quote~~~~~~~~
      (if (eq 2 2)
	  0
	0)
      ;;~~~~~~~~Cas if~~~~~~~~
      (if (eq 3 3)
	  0
	0)
      ;;~~~~~~~~Cas let~~~~~~~~
      (if (eq 4 4)
	  0
	0)
      ;;~~~~~~~~Cas Call~~~~~~~~
      (if (eq 1 1 )
	  0
	0)
      ;;~~~~~~~~Cas Special~~~~~~~~
      ;;special-form-p symbol
      ;;The function special-form-p takes a symbol. If the symbol globally names a special form, then a non-nil value is returned; otherwise nil is returned. A returned non-nil value is typically a function of implementation-dependent nature that can be used to interpret (evaluate) the special form.
      (if (speial-form-p fun)
	  (warn "NYI ~>" fun)
	(if (macro-function foo);;macro-function symbol
	    (lisp2li (macroExpend- expr))
	  
      )))


;;Map-Parcours
(defun map-parcours-lisp1 (exp env)
  (if (atom exp)
      0
    (+ (parcours-lisp1 (car exp)) (map-parcours-lisp1 (cdr exp) env))))

;;Get-Defun
(defun get-defun (symb)
  (get symb :defun))

;;Set-Defun
;;symb : expression evaluable mais pas evaluee
;;expr-lambda : expression evaluable evaluee
(defun set-defun (symb expr-lambda)
  (setf (get symb :defun)
	expr-lambda))






