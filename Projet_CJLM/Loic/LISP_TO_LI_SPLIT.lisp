;=============================================================================================================
;											LISP_TO_LI
;	<expr-li>
;	(:lit . <expr>)
;	(:var . <int>)
;	(:if <expr-li> <expr-li> . <expr-li>)
;	(:progn <expr-li> <expr-li>+)
;	(:set-var <int> . <expr-li>)
;	(:call <symbol> <expr-li>*)
;	(:mcall <symbol> <expr-li>*)
;	(:unknown <expr-eval> . <environ>)
; DEFUN
; LOOP
;   FOR
;   WHILE
;	(:let <int> <expr-li> <expr-li>+)
;	(:lclosure . <lambda-fun>)
;	(:set-fun <symbol> <expr-li>+)
;	(:apply <expr-li>*)
;	(:cvar <int> . <int>)
;	(:set-cvar <int> <int> . <expr-li>*)
;	(:lcall <int> <int> <expr-li>*)
;=============================================================================================================
;Make_env
(setf env_lisp_to_li_args (make-array 26))
(setf (aref env_lisp_to_li_args 0) (make-array 26))
(setf (aref env_lisp_to_li_args 0) 'A)
(setf (aref env_lisp_to_li_args 1) 'B)
(setf (aref env_lisp_to_li_args 2) 'C)
(setf (aref env_lisp_to_li_args 3) 'D)
(setf (aref env_lisp_to_li_args 4) 'E)
(setf (aref env_lisp_to_li_args 5) 'F)
(setf (aref env_lisp_to_li_args 6) 'G)
(setf (aref env_lisp_to_li_args 7) 'H)
(setf (aref env_lisp_to_li_args 8) 'I)
(setf (aref env_lisp_to_li_args 9) 'J)
(setf (aref env_lisp_to_li_args 10) 'K)
(setf (aref env_lisp_to_li_args 11) 'L)
(setf (aref env_lisp_to_li_args 12) 'M)
(setf (aref env_lisp_to_li_args 13) 'N)
(setf (aref env_lisp_to_li_args 14) 'O)
(setf (aref env_lisp_to_li_args 15) 'P)
(setf (aref env_lisp_to_li_args 16) 'Q)
(setf (aref env_lisp_to_li_args 17) 'R)
(setf (aref env_lisp_to_li_args 18) 'S)
(setf (aref env_lisp_to_li_args 19) 'T)
(setf (aref env_lisp_to_li_args 20) 'U)
(setf (aref env_lisp_to_li_args 21) 'V)
(setf (aref env_lisp_to_li_args 22) 'W)
(setf (aref env_lisp_to_li_args 23) 'X)
(setf (aref env_lisp_to_li_args 24) 'Y)
(setf (aref env_lisp_to_li_args 25) 'Z)

(setf env_lisp_to_li_varLocales (make-array 26))
(setf (aref env_lisp_to_li_varLocales 0) (make-array 26))
(setf (aref env_lisp_to_li_varLocales 0) 'AA)
(setf (aref env_lisp_to_li_varLocales 1) 'BA)
(setf (aref env_lisp_to_li_varLocales 2) 'CA)
(setf (aref env_lisp_to_li_varLocales 3) 'DA)
(setf (aref env_lisp_to_li_varLocales 4) 'EA)
(setf (aref env_lisp_to_li_varLocales 5) 'FA)
(setf (aref env_lisp_to_li_varLocales 6) 'GA)
(setf (aref env_lisp_to_li_varLocales 7) 'HA)
(setf (aref env_lisp_to_li_varLocales 8) 'IA)
(setf (aref env_lisp_to_li_varLocales 9) 'JA)
(setf (aref env_lisp_to_li_varLocales 10) 'KA)
(setf (aref env_lisp_to_li_varLocales 11) 'LA)
(setf (aref env_lisp_to_li_varLocales 12) 'MA)
(setf (aref env_lisp_to_li_varLocales 13) 'NA)
(setf (aref env_lisp_to_li_varLocales 14) 'OA)
(setf (aref env_lisp_to_li_varLocales 15) 'PA)
(setf (aref env_lisp_to_li_varLocales 16) 'QA)
(setf (aref env_lisp_to_li_varLocales 17) 'RA)
(setf (aref env_lisp_to_li_varLocales 18) 'SA)
(setf (aref env_lisp_to_li_varLocales 19) 'TA)
(setf (aref env_lisp_to_li_varLocales 20) 'UA)
(setf (aref env_lisp_to_li_varLocales 21) 'VA)
(setf (aref env_lisp_to_li_varLocales 22) 'WA)
(setf (aref env_lisp_to_li_varLocales 23) 'XA)
(setf (aref env_lisp_to_li_varLocales 24) 'YA)
(setf (aref env_lisp_to_li_varLocales 25) 'ZA)

(setf env_lisp_to_li (list env_lisp_to_li_args env_lisp_to_li_varLocales))

;(setf (aref env 0) '(A . 1))
;(setf (aref env 1) '(B . 1))
;(setf (aref env 2) '(C . 1))
;(setf (aref env 3) '(D . 1))
;(setf (aref env 4) '(E . 1))
;(setf (aref env 5) '(F . 1))
;(setf (aref env 6) '(G . 1))
;(setf (aref env 7) '(H . 1))
;(setf (aref env 8) '(I . 1))
;(setf (aref env 9) '(J . 1))
;(setf (aref env 10) '(K . 1))
;(setf (aref env 11) '(L . 1))
;(setf (aref env 12) '(M . 1))
;(setf (aref env 13) '(N . 1))
;(setf (aref env 14) '(O . 1))
;(setf (aref env 15) '(P . 1))
;(setf (aref env 16) '(Q . 1))
;(setf (aref env 17) '(R . 1))
;(setf (aref env 18) '(S . 1))
;(setf (aref env 19) '(T . 1))
;(setf (aref env 20) '(U . 1))
;(setf (aref env 21) '(V . 1))
;(setf (aref env 22) '(W . 1))
;(setf (aref env 23) '(X . 1))
;(setf (aref env 24) '(Y . 1))
;(setf (aref env 25) '(Z . 1))

;==========================
(defun MAP_LISP_TO_LI (lexpr env) 
  (if (atom lexpr)  
    NIL 
    (list* (LISP_TO_LI (first lexpr) env) (MAP_LISP_TO_LI (rest lexpr) env))))
;(trace MAP_LISP_TO_LI)
;==========================

;==========================
;;Get-Defun
(defun get_defun (symb)
  (get symb :defun))
;(trace get_defun)
;==========================

;==========================
;;Set-Defun
;;symb : expression evaluable mais pas evaluee
;;expr-lambda : expression evaluable evaluee
(defun set_defun (symb expr-lambda)
  (setf (get symb :defun)
    expr-lambda))
;(trace set_defun)
;========================== 

;==========================
;;search_multi_env

(defun search_multi_env (x y env)
  (if (atom env)
    (error "La variable ~s n'est pas connu dans l'environnement ~s" x env)
    (let  ((pos
          (position x (car env))))
    (if pos 
      (if (eq y 1)
        (cons :var (+ 1 pos))
        (list* :cvar y (+ 1 pos)))
      (search_multi_env x (+ 1 y) (cdr env))))))
(trace search_multi_env)
;==========================

;==========================
(defun LISP_TO_LI (expr env) 
  (if (atom expr) 
    (LISP_TO_LI_Atom expr env)
    (LISP_TO_LI_noAtom expr env)))
(trace LISP_TO_LI)
;==========================

;==========================V
(defun LISP_TO_LI_Atom (expr env)
  (if (constantp expr) 
    (LISP_TO_LI_atom_const expr env)
    (LISP_TO_LI_atom_var_cvar expr env)))
(trace LISP_TO_LI_Atom)
;==========================V

;==========================V
(defun LISP_TO_LI_atom_const (expr env)
  (cons :lit expr))
(trace LISP_TO_LI_atom_const)
;==========================V

;==========================V
(defun LISP_TO_LI_atom_var (expr env)
  ;
  (let ((pos
   (position expr env)))
  ; 
  (if pos 
    (cons :var (+ 1 pos)) 
    (error "La variable ~s n'est pas connu dans l'environnement ~s" expr env))))
;(trace LISP_TO_LI_atom_var)
;==========================V

;==========================V
(defun LISP_TO_LI_atom_var_cvar (expr env)
  ;
  (search_multi_env expr 1 env))
(trace LISP_TO_LI_atom_var_cvar)
;==========================V

;==========================X
(defun LISP_TO_LI_noAtom (expr env)
  ;
  (let ((fun (car expr)) 
   (args (cdr expr)))
  ;
  (cond
   ((eq 'quote fun)
    (LISP_TO_LI_noAtom_quote args env))
       ;
       ((eq 'if fun)
        (LISP_TO_LI_noAtom_if args env))
       ;
       ((eq 'cond fun)
        (LISP_TO_LI_noAtom_cond expr env))
       ;
       ((eq 'progn fun)
        (LISP_TO_LI_noAtom_progn args env))
       ;
       ((eq 'setf fun)
        (LISP_TO_LI_noAtom_set_var args env))
       ;
       ((get_defun fun)
          (LISP_TO_LI_noAtom_mcall fun args env))
       ;
       ((eq 'defun fun)
        (LISP_TO_LI_noAtom_defun args env))
       ;
       ((eq 'mcall fun)
        (LISP_TO_LI_noAtom_mcall fun args env))
       ;
       ((eq 'let fun)
         (LISP_TO_LI_noAtom_let args env))
       ;        
       ((eq 'lclosure fun)
        (LISP_TO_LI_noAtom_lclosure args env))
       ;
       ((eq 'set_fun fun)
        (LISP_TO_LI_noAtom_set_fun args env))
       ;
       ((eq 'apply fun)
        (LISP_TO_LI_noAtom_apply args env))
       ;
       ((eq 'cvar fun)
        (LISP_TO_LI_noAtom_cvar args env))
       ;
       ((eq 'set_cvar fun)
        (LISP_TO_LI_noAtom_cvar args env))
       ;
       ((eq 'lcall fun)
         (LISP_TO_LI_noAtom_lcall args env))
       ;;
       ((fboundp fun) 
        (LISP_TO_LI_noAtom_call fun args env))
       ;
       ((not (fboundp fun)) 
         (LISP_TO_LI_noAtom_unknown fun args env)))))
(trace LISP_TO_LI_noAtom)
;==========================X

;==========================X
(defun LISP_TO_LI_noAtom_quote (args env)
  )
;(trace LISP_TO_LI_noAtom_quote)
;==========================X

;==========================V~
(defun LISP_TO_LI_noAtom_if (args env)
  (list* :if (MAP_LISP_TO_LI args env)))
(trace LISP_TO_LI_noAtom_if)
;==========================V~

;==========================V~
(defun LISP_TO_LI_noAtom_cond (expr env)
  (LISP_TO_LI (macroexpand-1 expr) env))
(trace LISP_TO_LI_noAtom_cond)
;==========================V~

;==========================V
(defun LISP_TO_LI_noAtom_call (fun args env)
  (list* :call fun (MAP_LISP_TO_LI args env)))
(trace LISP_TO_LI_noAtom_call)
;==========================V

;==========================V~ traité cas err
(defun LISP_TO_LI_noAtom_set_var (args env)
  (if (symbolp (first args)) 
    (list :set_var (cdr (LISP_TO_LI (first args) env)) (LISP_TO_LI (second args) env)) 
    (list* :setf (LISP_TO_LI (first args ) env))))
(trace LISP_TO_LI_noAtom_set_var)
;==========================V ~

;==========================X
(defun LISP_TO_LI_noAtom_unknown (fun args env)
  (list :unknown (cons fun args) env))
(trace LISP_TO_LI_noAtom_unknown)
;==========================V

;==========================V
(defun LISP_TO_LI_noAtom_progn (args env)
  (list* :progn (MAP_LISP_TO_LI args env)))
(trace LISP_TO_LI_noAtom_progn)
;==========================V


;==========================V
(defun LISP_TO_LI_noAtom_defun_1 (args env)
  (list ':call 'set_defun (cons ':lit (first args)) (list ':lit ':lambda (length (second args)) (LISP_TO_LI (third args) (second args)))))
;(trace LISP_TO_LI_noAtom_defun)
;==========================V

;==========================V
(defun LISP_TO_LI_noAtom_defun (args env)
  (set_defun (first args) (list ':lit ':lambda (length (second args)) (LISP_TO_LI (third args) (second args)))))
(trace LISP_TO_LI_noAtom_defun)
;==========================V

;==========================X
(defun LISP_TO_LI_noAtom_mcall (fun args env)
  (list* :mcall fun (MAP_LISP_TO_LI args env)))
(trace LISP_TO_LI_noAtom_mcall)
;==========================X

;==========================X
(defun LISP_TO_LI_noAtom_let (args env)
  )
;(trace LISP_TO_LI_noAtom_let)
;==========================X

;==========================X
(defun LISP_TO_LI_noAtom_lcall (args env)
	)
;(trace LISP_TO_LI_noAtom_lcall)
;==========================X

;==========================X
(defun LISP_TO_LI_noAtom_set_cvar (args env)
	)
;(trace LISP_TO_LI_noAtom_set_cvar)
;==========================X

;==========================X
(defun LISP_TO_LI_noAtom_cvar (args env)
	)
;(trace LISP_TO_LI_noAtom_cvar)
;==========================X

;==========================X
(defun LISP_TO_LI_noAtom_apply (args env)
	)
;(trace LISP_TO_LI_noAtom_apply)
;==========================X

;==========================X
(defun LISP_TO_LI_noAtom_set_fun (args env)
	)
;(trace LISP_TO_LI_noAtom_set_fun)
;==========================X

;==========================X
(defun LISP_TO_LI_noAtom_lclosure (args env)
	)
;(trace LISP_TO_LI_noAtom_lclosure)
;==========================X











