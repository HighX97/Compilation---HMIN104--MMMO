;=============================================================================================================
;											EVAL_LI
;	<expr-li>
;	(:const . <expr>)
;	(:var . <int>)
;	(:if <expr-li> <expr-li> . <expr-li>)
;	(:progn <expr-li> <expr-li>+)
;	(:set-var <int> . <expr-li>)
;	(:call <symbol> <expr-li>*)
;	(:mcall <symbol> <expr-li>*)
;	(:unknown <expr-eval> . <environ>)
;
;	(:let <int> <expr-li> <expr-li>+)
;	(:lclosure . <lambda-fun>)
;	(:set-fun <symbol> <expr-li>+)
;	(:apply <expr-li>*)
;	(:cvar <int> . <int>)
;	(:set-cvar <int> <int> . <expr-li>*)
;	(:lcall <int> <int> <expr-li>*)
;=============================================================================================================

;==========================
(defun EVAL_LI_noAtom_lcall (args env)
	)
;==========================
(defun EVAL_LI_noAtom_set-cvar (args env)
	)
;==========================
(defun EVAL_LI_noAtom_cvar (args env)
	)
;==========================
(defun EVAL_LI_noAtom_apply (args env)
	)
;==========================
(defun EVAL_LI_noAtom_set-fun (args env)
	)
;==========================
(defun EVAL_LI_noAtom_lclosure (args env)
	)
;==========================
(defun EVAL_LI_noAtom_let (args env)
	)
;==========================
(defun EVAL_LI_noAtom_mcall (args env)
	)
;==========================
(defun EVAL_LI_noAtom_call (args env)
	)
;==========================
(defun EVAL_LI_noAtom_set-var (args env)
	)
;==========================
(defun EVAL_LI_noAtom_progn (args env)
	)
;==========================
(defun EVAL_LI_noAtom_if (args env)
	)
;==========================
(defun EVAL_LI_noAtom_quote (args env)
	)
;==========================
(defun EVAL_LI_noAtom (expr env)
	;
	(let ((fun (car expr)) 
	  (args (cdr expr)))
	;
	(cond
       ((eq 'quote fun)
       	(EVAL_LI_noAtom_quote (args env)))
       ;
       ((eq 'if fun)
       	(EVAL_LI_noAtom_if (args env)))
       ;
       ((eq 'progn fun)
       	(EVAL_LI_noAtom_progn (args env)))
       ;
       ((eq 'set-var fun)
       	(EVAL_LI_noAtom_set-var (args env)))
       ;
       ((eq 'call fun)
       	(EVAL_LI_noAtom_call (args env)))
       ;
       ((eq 'mcall fun)
       	(EVAL_LI_noAtom_mcall (args env)))
       ;
       ((eq 'let fun)
       (EVAL_LI_noAtom_let (args env)))
       ;       	
       ((eq 'lclosure fun)
       	(EVAL_LI_noAtom_lclosure (args env)))
       ;
       ((eq 'set-var fun)
       	(EVAL_LI_noAtom_set-var (args env)))
       ;
       ((eq 'set-fun fun)
       	(EVAL_LI_noAtom_set-fun (args env)))
       ;
       ((eq 'apply fun)
       	(EVAL_LI_noAtom_apply (args env)))
       ;
       ((eq 'cvar fun)
       	(EVAL_LI_noAtom_cvar (args env)))
       ;
       ((eq 'set-cvar fun)
       	(EVAL_LI_noAtom_cvar (args env)))
       ;
       ((eq 'lcall fun)
       (EVAL_LI_noAtom_lcall (args env))))))

;==========================
(defun EVAL_LI_atom_var (expr env)
	)
;==========================
(defun EVAL_LI_atom_const (expr env)
	
	)
;==========================
(defun EVAL_LI_atom (expr env)
	(if (constantp expr) 
		  (EVAL_LI_atom_const (expr env))
		  (EVAL_LI_atom_var (expr env))   
	))

;==========================
(defun EVAL_LI (expr env) 
  (if (atom expr) 
      (EVAL_LI_Atom (expr env))
      (EVAL_LI_noAtom (expr env)))   





