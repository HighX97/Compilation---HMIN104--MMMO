(:const . <expr>)
(:var . <int>)
(:if  <expr_li>  <expr_li>  <expr_li>)
(:progn  <expr_li> <expr_li>+)
(:set_var <int> . <expr_li>)
(:mcall  <symbol> <expr_li>*)
(:call  <symbol> <expr_li>*)
(:unknown  <symbol> <expr_li>*)

1- Exemple de langage intermédiaire (sur 6)

1-1
(defun mc (x)
    (if (> x 100)
        (- x 10)
        (mc (mc (+ x 100)))))

1-2
(:LIT :LAMBDA 1
 (:IF (:CALL > (:VAR . 1) (:LIT . 100))
     (:CALL - (:VAR . 1) (:LIT . 10))
  (:CALL MC (:CALL MC (:CALL + (:VAR . 1) (:LIT . 100))))))

 1-3
(SET_DEFUN 'MC
'(:LIT :LAMBDA 1
 (:IF (:CALL > (:VAR . 1) (:LIT . 100))
     (:CALL - (:VAR . 1) (:LIT . 10))
  (:MCALL MC (:MCALL MC (:CALL + (:VAR . 1) (:LIT . 100)))))))

1-4
(SET_DEFUN 'MCA
 '(:LIT :LAMBDA 1
   (:IF (:CALL > (:VAR . 1) (:LIT . 100))
       (:CALL - (:VAR . 1) (:LIT . 10))
       (:UNKNOWN (MCA (MCA (+ X 100))) (X)))))



2- Vérification de la syntaxe (sur 7)

(defun verifier_syntaxe_li (expr_li env)
    (let ((fun (car expr))
   (args (cdr expr)))
    (ecase (fun)
    (:LIT
      (if (not (atom args))
          (progn (warn "trop d'arguments pour le cas :LIT") nil)
          (progn  (print ":LIT ok") T)
    (:VAR
      (if (not (atom args))
           (progn (warn "trop d'arguments pour le cas :VAR") nil)
               (if (not (integerp args))
                   (progn (warn ":VAR doit être suivi par un entier naturel") nil)
                   (if (> 'args (array-dimensions env))
                       (progn (warn ":VAR la position de la variable est hors de l'environnement") nil)
                       (print ":VAR ok"))))
    (:SET_VAR
      (if (not (atom (car args)))
           (warn "trop d'arguments pour le cas :SET-VAR"))
               (if (not (integerp (car args)))
                   (warn ":SET-VAR doit être suivi par un entier naturel")
                   (if (> '(car args) (array-dimensions env))
                       (progn (warn ":SET-VAR la position de la variable est hors de l'environnement") nil)
                       (verifier_syntaxe_li (cdr args)))))
    (:IF
      (if (verifier_syntaxe_li (first args))
          (if (verifier_syntaxe_li (second args))
              (if (verifier_syntaxe_li (third args))
                  T
                  nil)
                nil)
              nil))
    (:CALL
      (if (fboundp (first args))
          (verifier_syntaxe_li (second args))
          nil))
    (:MCALL
      (if (get_defun (first args))
          (verifier_syntaxe_li (second args))
          nil))
    (:PROGN
      (if (not (atom args))
          (if (verifier_syntaxe_li (first args))
              (map_prong_verifier_syntaxe_li (cdr args) env)
              nil)
            nil))
    (:UNKNOWN
      (if (atom args)
          nil
          T))))))

(defun map_prong_verifier_syntaxe_li (expr env)
  (if (atom expr)
      NIL
      (if (atom (rest expr))
            (verifier_syntaxe_li (first expr) env)
            (map_prong_verifier_syntaxe_li (rest expr) env))))



3- Décompiltion (sur 10)

3-1

(defun li_to_lisp_novar (expr_li env)
    (let ((fun (car expr_li))
   (args (cdr expr_li)))
    (ecase (fun)
    (:LIT
        (if (verifier_syntaxe_li expr_li env)
            args))
    (:IF
        (if (verifier_syntaxe_li expr_li env)
            (list ‘progn (map_li_to_lisp_novar args env))))
    (:CALL
        (if (verifier_syntaxe_li expr_li env)
            (list (first args) (li_to_lisp_novar (rest args) env))))  
    (:MCALL
        (if (verifier_syntaxe_li expr_li env)
            (list (first args) (li_to_lisp_novar (rest args) env))))
    (:PROGN
        (if (verifier_syntaxe_li expr_li env)
            (list ‘progn (map_li_to_lisp_novar args env)))))))

(defun map_li_to_lisp_novar (lexpr env)
  (if (atom lexpr)  
    NIL
    (list* (li_to_lisp_novar (first lexpr) env) (map_li_to_lisp_novar (rest lexpr) env))))



3-2

(defun li_to_lisp_var (expr_li env)
    (let ((fun (car expr))
   (args (cdr expr)))
    (ecase (fun)
    (:VAR
        (if (verifier_syntaxe_li expr_li env)
            (if (get_elt env (first args))
            	(get_elt env (first args))
            	(progn (set_elt env (first args) (lispgensym)) (get_elt env (first args))))))
    (:SET_VAR
        (if (verifier_syntaxe_li expr_li env)
            (list 'setf (get_elt env (first args)) (li_to_lisp_var (rest args) env)))))))



3-3

(defun li_to_lisp_var_local (expr_li env)
    (let ((fun (car expr))
   (args (cdr expr)))
    (ecase (fun)
    (:UNKNOWN
    	))))



3-4

(defun li_to_lisp_unknown (expr_li env)
    (let ((fun (car expr))
   (args (cdr expr)))
    (ecase (fun)
    (:UNKNOWN
    	))))


