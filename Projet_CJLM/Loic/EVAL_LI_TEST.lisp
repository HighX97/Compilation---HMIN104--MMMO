;=============================================================================================================
;Test

;==========================
;in   : env_eval_li
;out  : #(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
;==========================

;==========================
;in   : (eval_li (lisp_to_li 1 env_lisp_to_li) env_eval_li)
;out  : 1

;in   : (eval_li (lisp_to_li 7/4 env_lisp_to_li) env_eval_li)
;out  : 7/4

;in   : (eval_li (lisp_to_li 1.75 env_lisp_to_li) env_eval_li)
;out  : 1.75

;in   : (eval_li (lisp_to_li "string" env_lisp_to_li) env_eval_li)
;out  : "string"
;==========================

;==========================
;       VAR
;in   : (eval_li  (lisp_to_li 'c env_lisp_to_li) env_eval_li)
;out  : c

;in   : (eval_li  (lisp_to_li 'k env_lisp_to_li) env_eval_li)
;out  : k

;in   : (eval_li  (lisp_to_li 'ua env_lisp_to_li) env_eval_li)
;out  : ua
;==========================


;==========================
;       SET_VAR
;env_eval_li : #(#(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z) #(AA BA CA DA EA FA GA HA IA JA KA LA MA NA OA PA QA RA SA TA UA VA WA XA YA ZA) NIL)
;in   : (eval_li (lisp_to_li '(setf c 1) env_lisp_to_li) env_eval_li)
;out  : 1
;in   : (eval_li  (lisp_to_li 'c env_lisp_to_li) env_eval_li)
;out  : 1
;env_eval_li : #(#(A B 1 D E F G H I J K L M N O P Q R S T U V W X Y Z) #(AA BA CA DA EA FA GA HA IA JA KA LA MA NA OA PA QA RA SA TA UA VA WA XA YA ZA) NIL)

;in   : (eval_li (lisp_to_li '(setf k c) env_lisp_to_li) env_eval_li)
;out  : 1
;in   : (eval_li  (lisp_to_li 'k env_lisp_to_li) env_eval_li)
;out  : 1
;env_eval_li : #(#(A B 1 D E F G H I J 1 L M N O P Q R S T U V W X Y Z) #(AA BA CA DA EA FA GA HA IA JA KA LA MA NA OA PA QA RA SA TA UA VA WA XA YA ZA) NIL)

;in   : (eval_li (lisp_to_li '(setf ua x) env_lisp_to_li) env_eval_li)
;out  : x
;in   : (eval_li  (lisp_to_li 'ua env_lisp_to_li) env_eval_li)
;out  : x
;env_eval_li : #(#(A B 1 D E F G H I J 1 L M N O P Q R S T U V W X Y Z) #(AA BA CA DA EA FA GA HA IA JA KA LA MA NA OA PA QA RA SA TA X VA WA XA YA ZA) NIL)
;==========================