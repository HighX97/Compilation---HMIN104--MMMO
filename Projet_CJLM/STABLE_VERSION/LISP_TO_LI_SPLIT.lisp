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
(setf env (make-array 26))
(setf (aref env 0) 'A)
(setf (aref env 1) 'B)
(setf (aref env 2) 'C)
(setf (aref env 3) 'D)
(setf (aref env 4) 'E)
(setf (aref env 5) 'F)
(setf (aref env 6) 'G)
(setf (aref env 7) 'H)
(setf (aref env 8) 'I)
(setf (aref env 9) 'J)
(setf (aref env 10) 'K)
(setf (aref env 11) 'L)
(setf (aref env 12) 'M)
(setf (aref env 13) 'N)
(setf (aref env 14) 'O)
(setf (aref env 15) 'P)
(setf (aref env 16) 'Q)
(setf (aref env 17) 'R)
(setf (aref env 18) 'S)
(setf (aref env 19) 'T)
(setf (aref env 20) 'U)
(setf (aref env 21) 'V)
(setf (aref env 22) 'W)
(setf (aref env 23) 'X)
(setf (aref env 24) 'Y)
(setf (aref env 25) 'Z)

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
(defun LISP_TO_LI (expr env) 
  (if (atom expr) 
    (LISP_TO_LI_Atom expr env)
    (LISP_TO_LI_noAtom expr env)))
(trace LISP_TO_LI)
;==========================

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

;==========================V
(defun LISP_TO_LI_Atom (expr env)
  (if (constantp expr) 
    (LISP_TO_LI_atom_const expr env)
    (LISP_TO_LI_atom_var expr env)))
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
    (cons :var pos) 
    (error "La variable ~s n'est pas connu dans l'environnement ~s" expr env))))
;(trace LISP_TO_LI_atom_var)
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
       ((eq 'defun fun)
        (LISP_TO_LI_noAtom_defun args env))
       ;
       ((eq 'mcall fun)
        (LISP_TO_LI_noAtom_mcall args env))
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
;(trace LISP_TO_LI_noAtom_if)
;==========================V~

;==========================V~
(defun LISP_TO_LI_noAtom_cond (expr env)
  (LISP_TO_LI (macroexpand-1 expr) env))
;(trace LISP_TO_LI_noAtom_if)
;==========================V~

;==========================V
(defun LISP_TO_LI_noAtom_call (fun args env)
  (list* :call fun (MAP_LISP_TO_LI args env)))
;(trace LISP_TO_LI_noAtom_call)
;==========================V

;==========================V~ traité cas err
(defun LISP_TO_LI_noAtom_set_var (args env)
  (if (symbolp (first args)) 
    (list :set_var (cdr (LISP_TO_LI (first args) env)) (LISP_TO_LI (second args) env)) 
    (list* :setf (LISP_TO_LI (first args ) env))))
;(trace LISP_TO_LI_noAtom_set_var)
;==========================V ~

;==========================X
(defun LISP_TO_LI_noAtom_unknown (fun args env)
  (list :unknown (cons fun args) env))
;(trace LISP_TO_LI_noAtom_unknown)
;==========================V

;==========================V
(defun LISP_TO_LI_noAtom_progn (args env)
  (list* :progn (MAP_LISP_TO_LI args env)))
(trace LISP_TO_LI_noAtom_progn)
;==========================V


;==========================V
(defun LISP_TO_LI_noAtom_defun (args env)
  (list ':call 'set_defun (cons ':lit (first args)) (list ':lit ':lambda (length (second args)) (LISP_TO_LI (third args) env))))
(trace LISP_TO_LI_noAtom_defun)
;==========================V

;==========================X
(defun LISP_TO_LI_noAtom_mcall (args env)
  )
;(trace LISP_TO_LI_noAtom_mcall)
;==========================X

;==========================X
(defun LISP_TO_LI_noAtom_mcall (args env)
  )
;(trace LISP_TO_LI_noAtom_mcall)
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

;==========================X
(defun LISP_TO_LI_noAtom_let (args env)
	)
;(trace LISP_TO_LI_noAtom_let)
;==========================X












