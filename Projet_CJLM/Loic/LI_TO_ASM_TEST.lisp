;=============================================================================================================
;Test

;==========================
;in   : (li_to_asm (lisp_to_li 1 env_lisp_to_li) 0)
;out  : ((MOVE (:LIT . 1) R0) (PUSH R0))

;in   : (li_to_asm (lisp_to_li 7/4 env_lisp_to_li) 0)
;out  : ((MOVE (:LIT . 7/4) R0) (PUSH R0))

;in   : (li_to_asm (lisp_to_li 1.75 env_lisp_to_li) 0)
;out  : ((MOVE (:LIT . 1.75) R0) (PUSH R0))

;in   : (li_to_asm (lisp_to_li "string" env_lisp_to_li) 0)
;out  : ((MOVE (:LIT . "string") R0) (PUSH R0))
;==========================

;==========================
;       VAR
;in   : (li_to_asm  (lisp_to_li 'c env_lisp_to_li) 26)
;out  : ((MOVE 23 'R2) (MOVE 'FP 'R1) (SUB 'R2 'R1) (LOAD 'R1 'R0))

;in   : (li_to_asm  (lisp_to_li 'k env_lisp_to_li) 26)
;out  : ((MOVE 15 'R2) (MOVE 'FP 'R1) (SUB 'R2 'R1) (LOAD 'R1 'R0))

;in   : (li_to_asm  (lisp_to_li 'ua env_lisp_to_li) 26)
;out  : ????
;==========================

;==========================
;       SET_VAR
;in   : (li_to_asm (lisp_to_li '(setf c 1) env_lisp_to_li) 26)
;out  : ((MOVE (:LIT . 1) 'R0) (MOVE 23 'R2) (MOVE 'FP 'R1) (SUB 'R2 'R1) (STORE 'R0 'R1))

;in   : (li_to_asm (lisp_to_li '(setf k c) env_lisp_to_li) 26)
;out  : ((MOVE (:VAR . 3) 'R0) (MOVE 15 'R2) (MOVE 'FP 'R1) (SUB 'R2 'R1) (STORE 'R0 'R1))

;in   : (li_to_asm (lisp_to_li '(setf ua x) env_lisp_to_li) 26)
;out  : ????
;==========================