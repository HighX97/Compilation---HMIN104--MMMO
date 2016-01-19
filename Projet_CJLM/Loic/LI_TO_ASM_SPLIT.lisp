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
                  (LI_TO_ASM_if (cdr expr) nbArgs))
            (:CALL 
                  (LI_TO_ASM_call (cdr expr) nbArgs))
            (:MCALL 
                  (LI_TO_ASM_mcall expr))
            (:PROGN 
                  (LI_TO_ASM_progn expr nbArgs))
            (:LET 
                  (LI_TO_ASM_let (cdr expr) nbArgs))
            (:WHILE 
                  (LI_TO_ASM_while (cdr expr) nbArgs))
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
;(:LIT . 0)
;MOVE  #<cste> R0
(defun LI_TO_ASM_const  (expr)
  (list 
    (list 'MOVE expr 'R0)))
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
  			(list 'MOVE decalage 'R2)
  			(list 'MOVE 'FP 'R1)
  			(list 'SUB 'R2 'R1)
  			(list 'LOAD  'R1 'R0)))))
;(trace LI_TO_ASM_var)
;==========================

;==========================
(defun LI_TO_ASM_cvar  (expr)
  )
;(trace LI_TO_ASM_cvar)
;==========================

;==========================
(setf id_label 0)

(defun get_id_label ()
  (setf id_label (+ 1 id_label))
  id_label)

;(:IF (:LIT . T) (:LIT . 1) (:LIT . 2))
;(:IF (:CALL < (:VAR . 24) (:VAR . 25)) (:VAR . 24) :VAR . 25)
;(:IF (:CALL NOT (:CALL EQ (:VAR . 24) (:VAR . 25))) (:VAR . 24) :VAR . 25)
;(:IF (:CALL OR (:CALL EQ (:VAR . 24) (:VAR . 25)) (:CALL EQ (:VAR . 25) (:VAR . 24))) (:VAR . 24) :VAR . 25)
(defun LI_TO_ASM_if  (expr nbArgs)
  (setf id_label_if (get_id_label))
  (append 
    (list
    (list 'LABEL (concatenate 'string "IF" (write-to-string id_label_if))))
    (if (eq (first (first expr)) ':call)
      (if (or (eq (second (first expr)) '<) 
              (eq (second (first expr)) '<=) 
              (eq (second (first expr)) '=) 
              (eq (second (first expr)) 'eq) 
              (eq (second (first expr)) 'eql) 
              (eq (second (first expr)) '>=) 
              (eq (second (first expr)) '>)
              (eq (second (first expr)) '/=))
        (append 
          ;(list 
          ;
          (LI_TO_ASM (third (first expr)) nbArgs)
          (list
          (list 'PUSH 'R0))
          ;
          (append  (LI_TO_ASM (fourth (first expr)) nbArgs)
          (list (list 'PUSH 'R0)
          ;
          (list 'MOVE 'SP 'R0)
          (list 'SUB 1 'R0)
          (list 'MOVE 'SP 'R1)
          (list 'SUB 2 'R1)
          ;
          (list 'CMP 'R0 'R1 )
        (cond
          ((eq (second (first expr)) '<)
              ;
              (list 'JLT (concatenate 'string "ELSE" (write-to-string id_label_if))))
              ;
          ((eq (second (first expr)) '<=)
              ;
              (list 'JLE (concatenate 'string "ELSE" (write-to-string id_label_if))))
          ((or (eq (second (first expr)) '=) (eq (second (first expr)) 'eql) (eq (second (first expr)) 'eq))
              ;
              (list 'JEQ (concatenate 'string "ELSE" (write-to-string id_label_if))))
         ((eq (second (first expr)) '>=)
              ;
              (list 'JGE (concatenate 'string "ELSE" (write-to-string id_label_if))))
          ((eq (second (first expr)) '>)
              ;
              (list 'JGT (concatenate 'string "ELSE" (write-to-string id_label_if))))
          ((eq (second (first expr)) '/=)
              ;
              (list 'JNE (concatenate 'string "ELSE" (write-to-string id_label_if))))))
          (append (LI_TO_ASM (second expr) nbArgs)
          (list (list 'JMP (concatenate 'string "FI" (write-to-string id_label_if)))
          (list 'LABEL (concatenate 'string "ELSE" (write-to-string id_label_if))))
          (append (LI_TO_ASM (third expr) nbArgs)
            (list
          (list 'LABEL (concatenate 'string "FI" (write-to-string id_label_if))))))))))))

(defun LI_TO_ASM_if_old  (expr nbArgs)
  (list
    (list 'LABEL 'IF)
    (list 'TEST (LI_TO_ASM (second expr) nbArgs))
    (list 'JNIL 'ELSE)
    (list (LI_TO_ASM (third expr) nbArgs))
    (list 'JMP 'FI)
    (list 'LABEL 'ELSE)
    (list (LI_TO_ASM (third expr) nbArgs))
    (list 'LABEL 'FI)))
(trace LI_TO_ASM_if)
;==========================

;==========================
;(:PROGN (:LIT . 1) (:LIT . 2) (:LIT . 3))

(defun LI_TO_ASM_progn  (expr nbArgs)
  (MAP_LI_TO_ASM_PROGN (cdr expr) nbArgs))
(trace LI_TO_ASM_progn)
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
;(:CALL + (:LIT . 1) (:LIT . 2))
;(+ (:LIT . 1) (:LIT . 2))
;(apply #'+ '(1 2))

(defun LI_TO_ASM_call  (expr nbArgs)
  (if (eq (first expr) 'SET-DEFUN)
    (LI_TO_ASM_defun (cdr expr) nbArgs)
    ;Push function name
    (append (list (list 'MOVE (list* :call (first expr)) 'R0)
    (list 'PUSH 'R0))
    ;Push args
    (MAP_LI_TO_ASM_CALL (cdr expr))
    ;Push nb_args
    (list (list 'MOVE (length (cdr expr)) 'R0)
    (list 'PUSH 'R0)
    (list 'MOVE 'SP 'FP)))))
(trace LI_TO_ASM_call)

(defun MAP_LI_TO_ASM_CALL (expr)
  (if (not (atom expr))
    (list*
      (list 'MOVE (car expr) 'R0)
      (list 'PUSH 'R0)
      (MAP_LI_TO_ASM_CALL (cdr expr)))))
(trace MAP_LI_TO_ASM_CALL)
;==========================
;(:CALL SET-DEFUN (:LIT . ADD) (:LIT :LAMBDA 2 (:CALL + (:VAR . 1) (:VAR . 2))))
(defun LI_TO_ASM_defun  (expr nbArgs)
  )
(trace LI_TO_ASM_defun)

(defun LI_TO_ASM_mcall  (expr)
  )
;(trace LI_TO_ASM_mcall)
;==========================

;==========================
;(:LET 2 ((:SET_VAR 4 (:LIT . 1)) (:SET_VAR 5 (:LIT . 2))))
(defun LI_TO_ASM_let (expr nbArgs)
  (let ((i (car expr)))
    (print expr)
    (print i)
    (print (cadr expr))
    (setf l nil)
    (loop for x in (cadr expr)
      do
        (setf l (append l (LI_TO_ASM x nbArgs))))
    l))
(trace LI_TO_ASM_let)
;==========================

;==========================
;(:WHILE (:CALL > (:VAR . 24) (:LIT . 0)) (:SET-VAR 24 :CALL - (:VAR . 24) (:LIT . 1)))

;(:WHILE (:CALL < (:VAR . 24) (:VAR . 25)) (:SET_VAR 24 (:CALL - (:VAR . 24) (:LIT . 1))))

(defun LI_TO_ASM_while (expr nbArgs)
  (setf id_label_if (get_id_label))
  (append 
    (list
    (list 'LABEL (concatenate 'string "WHILE" (write-to-string id_label_if))))
    (if (eq (first (first expr)) ':call)
      (if (or (eq (second (first expr)) '<) 
              (eq (second (first expr)) '<=) 
              (eq (second (first expr)) '=) 
              (eq (second (first expr)) 'eq) 
              (eq (second (first expr)) 'eql) 
              (eq (second (first expr)) '>=) 
              (eq (second (first expr)) '>)
              (eq (second (first expr)) '/=))
        (append 
          ;(list 
          ;
          (LI_TO_ASM (third (first expr)) nbArgs)
          (list
          (list 'PUSH 'R0))
          ;
          (append  (LI_TO_ASM (fourth (first expr)) nbArgs)
          (list (list 'PUSH 'R0)
          ;
          (list 'MOVE 'SP 'R0)
          (list 'SUB 1 'R0)
          (list 'MOVE 'SP 'R1)
          (list 'SUB 2 'R1)
          ;
          (list 'CMP 'R0 'R1 )
        (cond
          ((eq (second (first expr)) '<)
              ;
              (list 'JLT (concatenate 'string "CWHILE" (write-to-string id_label_if))))
              ;
          ((eq (second (first expr)) '<=)
              ;
              (list 'JLE (concatenate 'string "CWHILE" (write-to-string id_label_if))))
          ((or (eq (second (first expr)) '=) (eq (second (first expr)) 'eql) (eq (second (first expr)) 'eq)
              ;
              (list 'JEQ (concatenate 'string "CWHILE" (write-to-string id_label_if)))))
         ((eq (second (first expr)) '>=)
              ;
              (list 'JGE (concatenate 'string "CWHILE" (write-to-string id_label_if))))
          ((eq (second (first expr)) '>)
              ;
              (list 'JGT (concatenate 'string "CWHILE" (write-to-string id_label_if))))
          ((eq (second (first expr)) '/=)
              ;
              (list 'JNE (concatenate 'string "CWHILE" (write-to-string id_label_if)))))
          (list 'JMP (concatenate 'string "FWHILE" (write-to-string id_label_if)))
          (list 'LABEL (concatenate 'string "CWHILE" (write-to-string id_label_if))))
          (append (LI_TO_ASM (second expr) nbArgs)
            (list
              (list 'JMP (concatenate 'string "CWHILE" (write-to-string id_label_if)))
              (list 'LABEL (concatenate 'string "FWHILE" (write-to-string id_label_if)))))))))))
(trace LI_TO_ASM_while)
;==========================
;==========================

;==========================
(defun LI_TO_ASM_unknown  (expr)
  )
;(trace LI_TO_ASM_unknown)
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
(defun LI_TO_ASM_set_cvar  (expr)
  )
;(trace LI_TO_ASM_set_cvar)
;==========================

;==========================
(defun LI_TO_ASM_lcall  (expr)
  )
;(trace LI_TO_ASM_lcall)
;==========================








