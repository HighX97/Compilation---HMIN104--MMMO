;=============================================================================================================
;											LI_TO_ASM
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
(defun LI_TO_ASM_noAtom_lcall (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_set-cvar (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_cvar (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_apply (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_set-fun (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_lclosure (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_let (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_mcall (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_call (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_set-var (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_progn (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_if (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom_quote (args env)
	)
;==========================
(defun LI_TO_ASM_noAtom (expr env)
	;
	(let ((fun (car expr)) 
	  (args (cdr expr)))
	;
	(cond
       ((eq 'quote fun)
       	(LI_TO_ASM_noAtom_quote (args env)))
       ;
       ((eq 'if fun)
       	(LI_TO_ASM_noAtom_if (args env)))
       ;
       ((eq 'progn fun)
       	(LI_TO_ASM_noAtom_progn (args env)))
       ;
       ((eq 'set-var fun)
       	(LI_TO_ASM_noAtom_set-var (args env)))
       ;
       ((eq 'call fun)
       	(LI_TO_ASM_noAtom_call (args env)))
       ;
       ((eq 'mcall fun)
       	(LI_TO_ASM_noAtom_mcall (args env)))
       ;
       ((eq 'let fun)
       (LI_TO_ASM_noAtom_let (args env)))
       ;       	
       ((eq 'lclosure fun)
       	(LI_TO_ASM_noAtom_lclosure (args env)))
       ;
       ((eq 'set-var fun)
       	(LI_TO_ASM_noAtom_set-var (args env)))
       ;
       ((eq 'set-fun fun)
       	(LI_TO_ASM_noAtom_set-fun (args env)))
       ;
       ((eq 'apply fun)
       	(LI_TO_ASM_noAtom_apply (args env)))
       ;
       ((eq 'cvar fun)
       	(LI_TO_ASM_noAtom_cvar (args env)))
       ;
       ((eq 'set-cvar fun)
       	(LI_TO_ASM_noAtom_cvar (args env)))
       ;
       ((eq 'lcall fun)
       (LI_TO_ASM_noAtom_lcall (args env))))))

;==========================
(defun LI_TO_ASM_atom_var (expr env)
	)
;==========================
(defun LI_TO_ASM_atom_const (expr env)
	
	)
;==========================
(defun LI_TO_ASM_atom (expr env)
	(if (constantp expr) 
		  (LI_TO_ASM_atom_const (expr env))
		  (LI_TO_ASM_atom_var (expr env))   
	))

;==========================
(defun LI_TO_ASM (expr env) 
  (if (atom expr) 
      (LI_TO_ASM_Atom (expr env))
      (LI_TO_ASM_noAtom (expr env))) 