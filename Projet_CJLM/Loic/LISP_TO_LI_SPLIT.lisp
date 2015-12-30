;=============================================================================================================
;											LISP_TO_LI
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
(defun LISP2LI_noAtom_lcall (args env)
	)
;==========================
(defun LISP2LI_noAtom_set-cvar (args env)
	)
;==========================
(defun LISP2LI_noAtom_cvar (args env)
	)
;==========================
(defun LISP2LI_noAtom_apply (args env)
	)
;==========================
(defun LISP2LI_noAtom_set-fun (args env)
	)
;==========================
(defun LISP2LI_noAtom_lclosure (args env)
	)
;==========================
(defun LISP2LI_noAtom_let (args env)
	)
;==========================
(defun LISP2LI_noAtom_mcall (args env)
	)
;==========================
(defun LISP2LI_noAtom_call (args env)
	)
;==========================
(defun LISP2LI_noAtom_set-var (args env)
	)
;==========================
(defun LISP2LI_noAtom_progn (args env)
	)
;==========================
(defun LISP2LI_noAtom_if (args env)
	)
;==========================
(defun LISP2LI_noAtom_quote (args env)
	)
;==========================
(defun LISP2LI_noAtom (expr env)
	;
	(let ((fun (car expr)) 
	  (args (cdr expr)))
	;
	(cond
       ((eq 'quote fun)
       	(LISP2LI_noAtom_quote (args env)))
       ((eq 'if fun)
       	(LISP2LI_noAtom_if (args env)))
       ((eq 'progn fun)
       	(LISP2LI_noAtom_progn (args env)))
       ((eq 'set-var fun)
       	(LISP2LI_noAtom_set-var (args env)))
       ((eq 'call fun)
       	(LISP2LI_noAtom_call (args env)))
       ((eq 'mcall fun)
       	(LISP2LI_noAtom_mcall (args env)))
       ((eq 'let fun)
       (LISP2LI_noAtom_let (args env)))       	
       ((eq 'lclosure fun)
       	(LISP2LI_noAtom_lclosure (args env)))
       ((eq 'set-var fun)
       	(LISP2LI_noAtom_set-var (args env)))
       ((eq 'set-fun fun)
       	(LISP2LI_noAtom_set-fun (args env)))
       ((eq 'apply fun)
       	(LISP2LI_noAtom_apply (args env)))
       ((eq 'cvar fun)
       	(LISP2LI_noAtom_cvar (args env)))
       ((eq 'set-cvar fun)
       	(LISP2LI_noAtom_cvar (args env)))
       ((eq 'lcall fun)
       (LISP2LI_noAtom_lcall (args env))))))

;==========================
(defun LISP2LI_atom_var (expr env)
	)
;==========================
(defun LISP2LI_atom_const (expr env)
	
	)
;==========================
(defun LISP2LI_atom (expr env)
	(if (constantp expr) 
		  (LISP2LI_atom_const (expr env))
		  (LISP2LI_atom_var (expr env))   
	))

;==========================
(defun LISP2LI (expr env) 
  (if (atom expr) 
      (LISP2LI_Atom (expr env))
      (LISP2LI_noAtom (expr env)))   





