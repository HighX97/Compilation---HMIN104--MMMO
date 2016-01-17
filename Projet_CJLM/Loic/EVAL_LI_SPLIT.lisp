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
;Make_env
(setf env_eval_li_args (make-array 26))
(setf (aref env_eval_li_args 0) (make-array 26))
(setf (aref env_eval_li_args 0) 'A)
(setf (aref env_eval_li_args 1) 'B)
(setf (aref env_eval_li_args 2) 'C)
(setf (aref env_eval_li_args 3) 'D)
(setf (aref env_eval_li_args 4) 'E)
(setf (aref env_eval_li_args 5) 'F)
(setf (aref env_eval_li_args 6) 'G)
(setf (aref env_eval_li_args 7) 'H)
(setf (aref env_eval_li_args 8) 'I)
(setf (aref env_eval_li_args 9) 'J)
(setf (aref env_eval_li_args 10) 'K)
(setf (aref env_eval_li_args 11) 'L)
(setf (aref env_eval_li_args 12) 'M)
(setf (aref env_eval_li_args 13) 'N)
(setf (aref env_eval_li_args 14) 'O)
(setf (aref env_eval_li_args 15) 'P)
(setf (aref env_eval_li_args 16) 'Q)
(setf (aref env_eval_li_args 17) 'R)
(setf (aref env_eval_li_args 18) 'S)
(setf (aref env_eval_li_args 19) 'T)
(setf (aref env_eval_li_args 20) 'U)
(setf (aref env_eval_li_args 21) 'V)
(setf (aref env_eval_li_args 22) 'W)
(setf (aref env_eval_li_args 23) 'X)
(setf (aref env_eval_li_args 24) 'Y)
(setf (aref env_eval_li_args 25) 'Z)

(setf env_eval_li_varLocales (make-array 26))
(setf (aref env_eval_li_varLocales 0) (make-array 26))
(setf (aref env_eval_li_varLocales 0) 'AA)
(setf (aref env_eval_li_varLocales 1) 'BA)
(setf (aref env_eval_li_varLocales 2) 'CA)
(setf (aref env_eval_li_varLocales 3) 'DA)
(setf (aref env_eval_li_varLocales 4) 'EA)
(setf (aref env_eval_li_varLocales 5) 'FA)
(setf (aref env_eval_li_varLocales 6) 'GA)
(setf (aref env_eval_li_varLocales 7) 'HA)
(setf (aref env_eval_li_varLocales 8) 'IA)
(setf (aref env_eval_li_varLocales 9) 'JA)
(setf (aref env_eval_li_varLocales 10) 'KA)
(setf (aref env_eval_li_varLocales 11) 'LA)
(setf (aref env_eval_li_varLocales 12) 'MA)
(setf (aref env_eval_li_varLocales 13) 'NA)
(setf (aref env_eval_li_varLocales 14) 'OA)
(setf (aref env_eval_li_varLocales 15) 'PA)
(setf (aref env_eval_li_varLocales 16) 'QA)
(setf (aref env_eval_li_varLocales 17) 'RA)
(setf (aref env_eval_li_varLocales 18) 'SA)
(setf (aref env_eval_li_varLocales 19) 'TA)
(setf (aref env_eval_li_varLocales 20) 'UA)
(setf (aref env_eval_li_varLocales 21) 'VA)
(setf (aref env_eval_li_varLocales 22) 'WA)
(setf (aref env_eval_li_varLocales 23) 'XA)
(setf (aref env_eval_li_varLocales 24) 'YA)
(setf (aref env_eval_li_varLocales 25) 'ZA)

(setf env_eval_li (make-array 3))
(setf (aref env_eval_li 0) env_eval_li_args)
(setf (aref env_eval_li 1) env_eval_li_varLocales)
;(setf env_eval_li (make-array 26))
;(setf (aref env_eval_li 0) 'A)
;(setf (aref env_eval_li 1) 'B)
;(setf (aref env_eval_li 2) 'C)
;(setf (aref env_eval_li 3) 'D)
;(setf (aref env_eval_li 4) 'E)
;(setf (aref env_eval_li 5) 'F)
;(setf (aref env_eval_li 6) 'G)
;(setf (aref env_eval_li 7) 'H)
;(setf (aref env_eval_li 8) 'I)
;(setf (aref env_eval_li 9) 'J)
;(setf (aref env_eval_li 10) 'K)
;(setf (aref env_eval_li 11) 'L)
;(setf (aref env_eval_li 12) 'M)
;(setf (aref env_eval_li 13) 'N)
;(setf (aref env_eval_li 14) 'O)
;(setf (aref env_eval_li 15) 'P)
;(setf (aref env_eval_li 16) 'Q)
;(setf (aref env_eval_li 17) 'R)
;(setf (aref env_eval_li 18) 'S)
;(setf (aref env_eval_li 19) 'T)
;(setf (aref env_eval_li 20) 'U)
;(setf (aref env_eval_li 21) 'V)
;(setf (aref env_eval_li 22) 'W)
;(setf (aref env_eval_li 23) 'X)
;(setf (aref env_eval_li 24) 'Y)
;(setf (aref env_eval_li 25) 'Z)

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
(defun EVAL_LI_var_  (expr env)
  (aref env (- (cdr expr) 1)))
;(trace EVAL_LI_var)
;==========================

;==========================
(defun EVAL_LI_var  (expr env)
  (aref (aref env 0) (- (cdr expr) 1)))
(trace EVAL_LI_var)
;==========================

;==========================
(defun EVAL_LI_cvar  (expr env)
  (aref (aref env (- (cadr expr) 1)) (- (cddr expr) 1)))
;(trace EVAL_LI_cvar)
;(:CVAR 2 . 21)
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
(defun EVAL_LI_set_var_1  (expr env)
  (setf (aref env (- (cadr expr) 1)) 
      (EVAL_LI (caddr expr) env)))
;(trace EVAL_LI_set_var_)
;==========================

;==========================
(defun EVAL_LI_set_var  (expr env)
  (if (atom (cadr expr))
    (setf (aref (aref env 0) (- (cadr expr) 1)) 
      (EVAL_LI (caddr expr) env))
    (setf (aref (aref env (- (caadr expr) 1)) (- (cdadr expr) 1))
      (EVAL_LI (caddr expr) env))))
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
(defun EVAL_LI_set_cvar  (expr env)
  )
;(trace EVAL_LI_set_cvar)
;==========================

;==========================
(defun EVAL_LI_lcall  (expr env)
  )
;(trace EVAL_LI_lcall)
;==========================







