;=============================================================================================================
;											EVAL_LI  
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

;(LISP_TO_LI <LISP>) ---> <LI>
;(EVAL_LI <LI> = RESULT = (EVAL <LISP>)
;=============================================================================================================

(setf env_eval_li (make-array 26))
(setf (aref env_eval_li 0) 'A)
(setf (aref env_eval_li 1) 'B)
(setf (aref env_eval_li 2) 'C)
(setf (aref env_eval_li 3) 'D)
(setf (aref env_eval_li 4) 'E)
(setf (aref env_eval_li 5) 'F)
(setf (aref env_eval_li 6) 'G)
(setf (aref env_eval_li 7) 'H)
(setf (aref env_eval_li 8) 'I)
(setf (aref env_eval_li 9) 'J)
(setf (aref env_eval_li 10) 'K)
(setf (aref env_eval_li 11) 'L)
(setf (aref env_eval_li 12) 'M)
(setf (aref env_eval_li 13) 'N)
(setf (aref env_eval_li 14) 'O)
(setf (aref env_eval_li 15) 'P)
(setf (aref env_eval_li 16) 'Q)
(setf (aref env_eval_li 17) 'R)
(setf (aref env_eval_li 18) 'S)
(setf (aref env_eval_li 19) 'T)
(setf (aref env_eval_li 20) 'U)
(setf (aref env_eval_li 21) 'V)
(setf (aref env_eval_li 22) 'W)
(setf (aref env_eval_li 23) 'X)
(setf (aref env_eval_li 24) 'Y)
(setf (aref env_eval_li 25) 'Z)

;==========================
(defun EVAL_LI (expr env) 
  (ecase (car expr)
            (:LIT 
                  (EVAL_LI_const expr env))
            (:VAR 
                  (EVAL_LI_var expr env))
            (:SET_VAR 
                  (EVAL_LI_set_var expr env))
            (:IF 
                  (EVAL_LI_if expr env))
            (:CALL 
                  (EVAL_LI_call expr env))
            (:LAMBDA 
                  (EVAL_LI_lambda expr env))
            (:MCALL 
                  (EVAL_LI_mcall expr env))
            (:PROGN 
                  (EVAL_LI_progn expr env))
            (:LET 
                  (EVAL_LI_let expr env))
            (:LCLOSURE 
                  (EVAL_LI_lclosure expr env))
            (:SET_FUN 
                  (EVAL_LI_set_fun expr env))
            (:APPLY 
                  (EVAL_LI_apply expr env))
            (:CVAR 
                  (EVAL_LI_cvar expr env))
            (:SET_CVAR 
                  (EVAL_LI_set_cvar expr env))
            (:LCALL 
                  (EVAL_LI_lcall expr env))
            (:UNKNOWN 
                  (EVAL_LI_unknown expr env))))
(trace EVAL_LI)
;==========================

;==========================
(defun MAP_EVAL_LI (lexpr env) 
  (if (atom lexpr)  
    NIL 
    (list* (EVAL_LI (first lexpr) env) (MAP_EVAL_LI (rest lexpr) env))))
(trace MAP_EVAL_LI)
;==========================

;==========================                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
(defun MAP_EVAL_LI_PROGN (expr env)
  (if (atom expr) 
      NIL 
      (if (atom (rest expr))
            (EVAL_LI (first expr) env)
            (MAP_EVAL_LI_PROGN (rest expr) env))))
(trace MAP_EVAL_LI_PROGN)
;==========================

;==========================                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
(defun MAKE_ENV_EVAL_LI__ (nbArgs listArgs)
  (make_env_rec listArgs 0 (make-array (+ nbArgs 1))))
;(trace MAKE_ENV_EVAL_LI)
;==========================

;==========================                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
(defun MAKE_ENV_EVAL_LI (nbArgs listArgs)
  (if 
    (null args)
    nenv
    (progn
      (setf (aref nenv index) (car args))
      (make-env-eval-li (cdr args) env nenv (+ 1 index)))))
;(trace MAKE_ENV_EVAL_LI)
;==========================

;==========================
(defun MAKE_ENV_REC (listArgs pos envGenerated)
      (when listArgs
            (setf (aref envGenerated pos) (car listArgs))
            (make_env_rec (cdr listArgs) (+ pos 1) envGenerated))
      envGenerated)
;==========================

;==========================                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
(defun DISPLACE (cell1 cell2)
      (setf (car cell2) (car cell1)
            (cdr cell2) (car cell1))
      cell2)
;==========================
                                                                                                                                                                                                                                                                                              
(defun map-eval-LI-progn (expr env)
  (car (last (map-eval-LI expr env))))
                                                
(defun make-env-eval-li (args env nenv index) 
  (if 
    (null args)
    nenv
    (progn
      (setf (aref nenv index) (car args))
      (make-env-eval-li (cdr args) env nenv (+ 1 index))
      ))) 
 
                                                


;==========================
;;Get-Defun
(defun get_defun (symb)
  (get symb :defun))
(trace get_defun)
;==========================

;==========================
;;Set-Defun
;;symb : expression evaluable mais pas evaluee
;;expr-lambda : expression evaluable evaluee
(defun set_defun (symb expr-lambda)
  (setf (get symb :defun)
    expr-lambda))
(trace set_defun)
;========================== 

;==========================
(defun EVAL_LI_const  (expr env)
  (cdr expr))
(trace EVAL_LI_const)
;==========================

;==========================
(defun EVAL_LI_var  (expr env)
  (aref env (cdr expr)))
(trace EVAL_LI_var)
;==========================

;==========================
(defun EVAL_LI_if  (expr env)
  (if (EVAL_LI (second expr) env) 
                        (EVAL_LI (third expr) env)
                        (EVAL_LI (cadddr expr) env)))
;(trace EVAL_LI_if)
;==========================

;==========================
(defun EVAL_LI_progn  (expr env)
  (MAP_EVAL_LI_PROGN (cdr expr) env))
(trace EVAL_LI_progn)
;==========================

;==========================
(defun EVAL_LI_set_var  (expr env)
  (setf (aref env (- (cadr expr) 1)) 
      (EVAL_LI (caddr expr) env)))
(trace EVAL_LI_set_var)
;==========================

;==========================
(defun EVAL_LI_mcall_  (expr env)
  (let ((fun (get_defun (second expr)))
      (args (MAP_EVAL_LI (cddr expr) env)))
                                                ;(thrid de fun) : corp de la fonction
                                                ;(second fun) : parametres
                                                ;(get-defun 'fibo') : 
      (EVAL_LI (cddr fun)
        (MAKE_ENV_EVAL_LI (second fun) args))))
;(trace EVAL_LI_mcall)
;==========================

(defun EVAL_LI_mcall  (expr env)
  (let* ((fun (get_defun (second expr))))
      ;(args (map-eval-LI (cddr expr) env)))
      ;
      (if (eq (car (cddr expr)) :LIT)
        (let ((args (EVAL_LI (cddr expr) env))) 
          (EVAL_LI (third fun) (MAKE_ENV_EVAL_LI args env (make-array (+ 1 (cadr fun))) 1)))
            (let ((args (cons (EVAL_LI (car (cddr expr)) env)
              (MAP_EVAL_LI (cdr (cddr expr)) env))))
              (EVAL_LI (third fun)
        (MAKE_ENV_EVAL_LI args env (make-array (+ 1 (cadr fun))) 1))))))
;(trace EVAL_LI_mcall)
;==========================

(defun EVAL_LI_lambda  (expr env)
  (:LAMBDA 
      (MAP_EVAL_LI (cddr expr) env)))
(trace EVAL_LI_lambda)
;==========================

;==========================
(defun EVAL_LI_call  (expr env)
  (apply (second expr)
      (MAP_EVAL_LI (cddr expr) env)))
(trace EVAL_LI_call)
;==========================

;==========================
(defun EVAL_LI_unknown  (expr env)
  )
;(trace EVAL_LI_unknown)
;==========================

;==========================
(defun EVAL_LI_let  (expr env)
  )
;(trace EVAL_LI_let)
;==========================

;==========================
(defun EVAL_LI_lclosure  (expr env)
  )
;(trace EVAL_LI_lclosure)
;==========================

;==========================
(defun EVAL_LI_set_fun  (expr env)
  )
;(trace EVAL_LI_set_fun)
;==========================

;==========================
(defun EVAL_LI_apply  (expr env)
  )
;(trace EVAL_LI_apply)
;==========================

;==========================
(defun EVAL_LI_cvar  (expr env)
  )
;(trace EVAL_LI_cvar)
;==========================

;==========================
(defun EVAL_LI_set_cvar  (expr env)
  )
;(trace EVAL_LI_set_cvar)
;==========================

;==========================
(defun EVAL_LI_lcall  (expr env)
  )
;(trace EVAL_LI_lcall)
;==========================







