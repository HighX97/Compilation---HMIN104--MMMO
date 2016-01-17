;=============================================================================================================
;Test
;==========================
;in   : (vm_init 'vm)
;out  : #S(HASH-TABLE :TEST FASTHASH-EQL)
;==========================

;==========================
;in   : (vm_state 'vm)
;out  : "=====Registres=====" 
;       "===Registres généraux===" 
;       "RO : NIL" 
;       "R1 : NIL" 
;       "R2 : NIL" 
;       "===Registres dédiés===" 
;       "SP : 10" 
;       "BP : 10" 
;       "FP : 10" 
;       "PC : 10" 
;       "=====Drapeaux=====" 
;       "FLT : NIL" 
;       "FEQ : NIL" 
;       "FGT : NIL" 
;       "FNIL : NIL" 
;       "=====Memory=====" 
;       "memory : (vm_state_memory 'VM)" 
;       "=====HashTab_etq=====" 
;       "hashTab_etq : (vm_state_hashTab_etq 'VM)" 
;       "" 
;       ""
;==========================

;==========================
;in   : (vm_state_memory 'VM)
;out  : "memory : " 
;       #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;==========================

;==========================
;in   : (vm_state_hashTab_etq 'VM)
;out  : "hashTab_etq :" 
;       #S(HASH-TABLE :TEST FASTHASH-EQL)
;==========================

;==========================
;in   : (vm_read_asm 'vm (li_to_asm (lisp_to_li 1 env_lisp_to_li) 0))
;out  : "RO : (:LIT . 1)"
;		memory : #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL (:LIT . 1) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)

;in   : (vm_read_asm 'vm (li_to_asm (lisp_to_li 7/4 env_lisp_to_li) 0))
;out  : "RO : (:LIT . 7/4)"
;		memory : #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL (:LIT . 1) (:LIT . 7/4) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)

;in   : (vm_read_asm 'vm (li_to_asm (lisp_to_li 1.75 env_lisp_to_li) 0))
;out  : "RO : (:LIT . 1.75)"
;		memory : #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL (:LIT . 1) (:LIT . 7/4) (:LIT . 1.75) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)

;in   : (vm_read_asm 'vm (li_to_asm (lisp_to_li "string" env_lisp_to_li) 0))
;out  : "RO : (:LIT . \"string\")"
;		memory : #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL (:LIT . 1) (:LIT . 7/4) (:LIT . 1.75) (:LIT . "string") NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
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