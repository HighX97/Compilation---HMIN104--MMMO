;=============================================================================================================
;											LI_TO_ASM
;	<expr-li>
;	(:lit . <expr>) 						
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
(defun LI_TO_ASM (expr nbArgs) 
  	(ecase (car expr)
            (:LIT
                  (LI_TO_ASM_const expr))
            (:VAR 
                  (LI_TO_ASM_var expr nbArgs))
            (:SET_VAR 
                  (LI_TO_ASM_set_var expr nbArgs))
            (:IF 
                  (LI_TO_ASM_if expr nbArgs))
            (:CALL 
                  (LI_TO_ASM_call expr))
            (:MCALL 
                  (LI_TO_ASM_mcall expr))
            (:PROGN 
                  (LI_TO_ASM_progn expr nbArgs))
            (:LET 
                  (LI_TO_ASM_let expr))
            (:LCLOSURE 
                  (LI_TO_ASM_lclosure expr))
            (:SET_FUN 
                  (LI_TO_ASM_set_fun expr))
            (:APPLY 
                  (LI_TO_ASM_apply expr))
            (:CVAR 
                  (LI_TO_ASM_cvar expr))
            (:SET_CVAR 
                  (LI_TO_ASM_set_cvar expr))
            (:LCALL 
                  (LI_TO_ASM_lcall expr))
            (:UNKNOWN 
                  (LI_TO_ASM_unknown expr))))
(trace LI_TO_ASM)
;==========================

;==========================
(defun MAP_LI_TO_ASM (lexpr) 
  (if (atom lexpr)  
    NIL 
    (list* (LI_TO_ASM (first lexpr)) (MAP_LI_TO_ASM (rest lexpr)))))
;(trace MAP_LI_TO_ASM)
;==========================

;==========================                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
(defun MAP_LI_TO_ASM_PROGN (expr nbArgs)
  (if (atom  expr) 
      NIL 
      (list* (LI_TO_ASM (first expr) nbArgs)
      (MAP_LI_TO_ASM_PROGN (rest expr) nbArgs))))
(trace MAP_LI_TO_ASM_PROGN)
;==========================

;==========================
;(:LIT . 0)
;MOVE  #<cste> R0
(defun LI_TO_ASM_const  (expr)
  (list 'MOVE expr ''R0))
;(trace LI_TO_ASM_const)
;==========================

;==========================
;(:VAR . 0)
;LOAD  @<varg> R0
(defun LI_TO_ASM_var  (expr nbArgs)
  (if (> (cdr expr) nbArgs)
  	(warn "")
  	(let ((decalage (- nbArgs (cdr expr))))
  		(list
  			(list 'MOVE decalage ''R2)
  			(list 'MOVE ''FP ''R1)
  			(list 'SUB ''R2 ''R1)
  			(list 'LOAD  ''R1 ''R0)))))
;(trace LI_TO_ASM_var)
;==========================

;==========================
;(:IF (:LIT . T) (:LIT . 1) (:LIT . 2))
;
(defun LI_TO_ASM_if  (expr nbArgs)
  (list
  	(list 'LABEL 'IF)
  	(list 'TEST (LI_TO_ASM (second expr) nbArgs))
  	(list 'JNIL 'ELSE)
  	(list (LI_TO_ASM (third expr) nbArgs))
  	(list 'JMP 'FI)
  	(list 'LABEL 'ELSE)
  	(list (LI_TO_ASM (third expr) nbArgs))
  	(list 'LABEL 'FI)))
;(trace LI_TO_ASM_if)
;==========================

;==========================
;(:PROGN (:LIT . 1) (:LIT . 2) (:LIT . 3))

(defun LI_TO_ASM_progn  (expr nbArgs)
  (MAP_LI_TO_ASM_PROGN (cdr expr) nbArgs))
(trace LI_TO_ASM_progn)
;==========================

;==========================
;(:SET_VAR 0 (:LIT . 1))
(defun LI_TO_ASM_set_var (expr nbArgs)
  (if (> (second expr) nbArgs)
  	(warn "")
  	(let ((decalage (- nbArgs (second expr))))
  		(list
  		(list 'MOVE (third expr) ''R0)
  		(list 'MOVE decalage ''R2)
		(list 'MOVE ''FP ''R1)
		(list 'SUB ''R2 ''R1)
  		(list 'STORE ''R0 ''R1)))))
;(trace LI_TO_ASM_set_var)
;==========================

;==========================
(defun LI_TO_ASM_mcall  (expr)
  )
;(trace LI_TO_ASM_mcall)
;==========================

;==========================
(defun LI_TO_ASM_call  (expr)
  )
;(trace LI_TO_ASM_call)
;==========================

;==========================
(defun LI_TO_ASM_unknown  (expr)
  )
;(trace LI_TO_ASM_unknown)
;==========================

;==========================
(defun LI_TO_ASM_let  (expr)
  )
;(trace LI_TO_ASM_let)
;==========================

;==========================
(defun LI_TO_ASM_lclosure  (expr)
  )
;(trace LI_TO_ASM_lclosure)
;==========================

;==========================
(defun LI_TO_ASM_set_fun  (expr)
  )
;(trace LI_TO_ASM_set_fun)
;==========================

;==========================
(defun LI_TO_ASM_apply  (expr)
  )
;(trace LI_TO_ASM_apply)
;==========================

;==========================
(defun LI_TO_ASM_cvar  (expr)
  )
;(trace LI_TO_ASM_cvar)
;==========================

;==========================
(defun LI_TO_ASM_set_cvar  (expr)
  )
;(trace LI_TO_ASM_set_cvar)
;==========================

;==========================
(defun LI_TO_ASM_lcall  (expr)
  )
;(trace LI_TO_ASM_lcall)
;==========================







