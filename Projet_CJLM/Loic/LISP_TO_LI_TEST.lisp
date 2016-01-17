;=============================================================================================================
;Test

;==========================
;in   : env_lisp_to_li
;out  : #(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
;==========================

;==========================
;       LIT
;in   : (lisp_to_li 1 env_lisp_to_li)
;out  : (:LIT . 1)

;in   : (lisp_to_li 7/4 env_lisp_to_li)
;out  : (:LIT . 7/4)

;in   : (lisp_to_li 1.75 env_lisp_to_li)
;out  : (:LIT . 1.75)

;in   : (lisp_to_li "string" env_lisp_to_li)
;out  : (:LIT . "string")
;==========================

;==========================
;       VAR
;in   : (lisp_to_li 'c env_lisp_to_li)
;out  : (:VAR . 3)

;in   : (lisp_to_li 'k env_lisp_to_li)
;out  : (:VAR . 11)

;in   : (lisp_to_li 'ua env_lisp_to_li)
;out  : (:CVAR 2 . 21)
;==========================

;==========================
;       SET_VAR
;in   : (lisp_to_li '(setf c 1) env_lisp_to_li)
;out  : (:SET_VAR 3 (:LIT . 1))

;in   : (lisp_to_li '(setf k c) env_lisp_to_li)
;out  : (:SET_VAR 11 (:VAR . 3))

;in   : (lisp_to_li '(setf ua x) env_lisp_to_li)
;out  : (:SET_VAR (2 . 21) (:VAR . 24))
;==========================
