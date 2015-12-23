; 3 Problemes :
; probleme avec listes 
; probleme liste :LIT
; l'autre probleme je ne sais plus je vais essayer de me rappeller

;faire attention au cas du if

(defun li2asm (expr param)
  (ecase (car expr)
	 (:LIT (list 'MOVE expr 'R0))
	 (:VAR (if (<= (cdr expr) param)
		   (let ((decalage (- param (- (cdr expr) 1)))) 
		     (list 'LOAD  (list '- 'FP decalage) 'R0))
		 (let ((decalage (+ 3 (- (cdr expr) param))))
		   (list 'LOAD (list '+ 'FP decalage) 'R0))))
	 (:IF  (list 'MOVE (li2asm (first  args)) R0 	;Break 6 [11]> (list 'MOVE (eq 1 1) 'R0)
				PUSH R0 
				MOVE (li2asm(second args ) R0)
				PUSH R0

				
				JNE ELSE 
				JEQ VRAI 
				( LABEL  VRAI )
				litoASM(second args ) R0
				( Label  ELSE )


													;(MOVE T R0)
													;Break 6 [11]> (list (eq 1 2) 'RO)
fun =(firt exprLi)
args=(rest exprLi)
(eq fun'if)
MOVE litoASM (first args) R0
CMP R0 True
JNE Else
litoASM(second args)
JMP Fi
litoASM(third args)
Fi

(NIL RO)
	 	()
	 (:CALL (if (eq (second expr) '+)
		   ; (if (< 2 (length (cddr expr)))
			(li2asm-add (cddr expr) param)
		    ;(list 'ADD (list :LIT (cdr (third expr))) (list :LIT (cdr (fourth expr)))))
		  (warn "autre cas")))
))


;(defun li2asm-add (expr param)
;  (if (atom expr)
;      nil
;    (if (eq (caar expr) :CALL)
;	(list (li2asm (car expr) param)
;	    (list 'ADD  ))))
	 ;(list 'MOVE 'R0 'R1)))))

(defun li2asm-add (expr param)
  (if (eq (caar expr) :CALL)
      (if (eq (caadr expr) :CALL)
	  (list (li2asm (car expr) param) 
		(list 'MOVE 'R0 'R1) 
		(li2asm (cadr expr) param) 
		(list 'ADD 'R1 'R0))
	(if (eq (caadr expr) :LIT)
	    (list (li2asm (car expr) param)
		  (list 'MOVE 'R0 'R1)
		  (li2asm (cadr expr) param)
		  (list 'ADD 'R1 'R0))
	  (if (eq (cadr expr) 'R0)
	      (list (li2asm (car expr) param)
		    (list 'MOVE 'R0 'R1)
		    (list 'ADD 'R1 'R0))
	    (list (list 'MOVE (cadr expr) 'R0)
		  (li2asm (car expr) param)
		  (list 'MOVE 'R0 'R1)
		  (list 'ADD 'R1 'R0)))))
    (if (eq (caar expr) :LIT)
	(if (eq (caadr expr) :CALL)
	    (list (li2asm (cadr expr) param) 
		  (list 'ADD (caar expr) 'R0))
	  (if (eq (caadr expr) :LIT)
	      (list (list 'MOVE (cadr expr) 'RO)
		    (list 'ADD (car expr) 'R0))
	    (if (eq (cadr expr) 'R0)
		(list 'ADD (caar expr) (cadr expr))
	      (list (list 'MOVE (cadr expr) 'R0)
		    (list 'ADD (caar expr) 'R0))))))
	    ))
      
      
      
      
      