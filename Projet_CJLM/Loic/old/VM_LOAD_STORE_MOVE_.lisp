;=============================================================================================================
;												VM
;La machine virtuelle que nous utilisons dispose d'une mémoire (fini, mais suffisement grande afin de ne pas être géné) et de trois registres R0, R1 et R2. 
;La mémoire est un ensemble fini de N cellules. La première cellule a par définition le numéro 0, la dernière le numéro N-1. 
;On parlera plutôt d'adresse mémoire que de numéro de cellule.

;La pile est gérée dans la mémoire. La pile commence à l'adresse indique dans le registre spécial BP (base pointer). 
;A priori, on ne touchera jamais à ce registre. le sommet de pile est indiqué dans le registre spécial SP (stack pointer). 
;La pile est vide quand SP = BP. On a toujours SP >= BP (la pile est montante). Le compteur de programme est contenu dans un registre spécial PC (program counter).

;Pour l'instant, nous avons donc les registres suivants : R0, R1, R2, BP, SP, PC
;Nous introduirons dans la suite, un pointeur de cadre (frame pointer) : 
;FP. Pour les comparaison et les sauts conditionnel, on dispose de trois registres booléens (1 bit) appelés drapeaux (flag) : 
;DPP, DE, DPG (pour drapeau plus petit, drapeau égal, drapeau plus grand).

;Un adressage correspond soit à un source <src> d'où est lue une valeur, soit à une destination <dest> où une valeur est écrite.

;	:CONST - :LIT
;Une valeur constante s'écrit directement précédée par le symbole #
;Une valeur constante ne peut être qu'une source (une telle destination n'aurait pas de sens).

; (LOAD <src> <dest>)	=	chargement de mémoire à registre
; (STORE <src> <dest>	=	chargement de registre à mémoire
; (MOVE <src> <dest>)	=	mouvement de registre à registre
; (ADD <src> <dest>)	=	addition
; (SUB <src> <dest>)	=	soustraction
; (MUL <src> <dest>)	=	multiplication
; (DIV <src> <dest>)	=	division
; (INCR <dest>)			=	incrément
; (DECR <dest>)			=	décrément
; (PUSH <src>)			=	empiler
; (POP <dest>)			=	dépiler
; (LABEL <label>)		=	déclaration d’étiquette
; (JMP <label>)			=	saut inconditionnel à une étiquette
; (JSR <label>)			=	saut avec retour
; (RTN)					=	retour
; (CMP <src1> <src2>)	=	comparaison
; (JGT <label>)			=	saut si plus grand
; (JGE <label>)			=	saut si plus grand ou égal
; (JLT <label>)			=	saut si plus petit
; (JLE <label>)			=	saut si plus petit ou égal
; (JEQ <label>)			=	saut si égal
; (JNE <label>)			=	saut si différent
; (TEST <src>)			=	comparaison à NIL
; (JTRUE <label>)		=	saut si non-NIL
; (JNIL <label>)  		=	saut si NIL
; (NOP)					=	rien
; (HALT)				=	arrêt
;
;=============================================================================================================
(setq liste_registre '(R0 R1 R2 SP BP FP CO))
(setq mem_size_default 20)

(defun is_register_? (registre)
  (position registre liste_registre))

(defun make-vm (vm &optional size)
	(if (null size)
		(setf size mem_size_default))
    (set-memory vm size)
  	;(setf (get vm :debCode) (- size (* (floor (/ size 3)) 2)));depend de size 
  	;(setf (get vm :adrCode) (get vm :debCode));adresse pour charger dans le code
  	;(setf (get vm :FPP) 0);drapeau plus petit
  	;(setf (get vm :FPG) 0);drapeau plus grand
  	;(setf (get vm :FEQ) 0);drapeau egal
  	;(setf (get vm :ON) 0);interrupteur de la VM  on / off
    (setf (get vm :memory_size) size)
  	(set-register vm 'R0 nil)
  	(set-register vm 'R1 nil)
  	(set-register vm 'R2 nil)
  	(set-register vm 'SP 0)
  	(set-register vm 'BP 0)
  	(set-register vm 'FP 0) 
  	(set-register vm 'CO (- size (* (floor (/ size 3)) 2)))
  	(setf (get vm 'TRA) (make-hash-table)) ;table pour les ref en avance
  	(setf (get vm 'TSR) (make-hash-table))
) 

(defun set-memory (vm size)
	(setf (get vm :memory) (make-array size)))

;permet d'acceder à la valeur du registre
(defun get-register (vm registre)
  (get vm registre))

;permet de changer la valeur du registre
(defun set-register (vm valeur registre)
  (setf (get vm registre) valeur))	


;======================================================  
; (LOAD <src> <dest>) = chargement de mémoire à registre
(defun vm_load (vm src dest)
  (if (not (is_register_? dest))
    (warn "ERR : <dest> doit être un registre")
    (if (not (constantp src))
      (warn "ERR : <src> doit être une adresse mémoire (int)")
      (if (< (- (get vm :memory_size) 1) src)
        (warn (concatenate 'string "ERR : <src> l'adresse mémoire @" (write-to-string src) " est hors limites [0 , " (write-to-string (- (get vm :memory_size) 1)) "]"))
        (let ((getSrc
          (svref (get vm :memory) src))) 
          (if (not getSrc)
            (warn "ERR : <src> l'emplacement mémoire est vide")
            (set-register vm getSrc dest)))))))
;======================================================  

;======================================================  
; (STORE <src> <dest> = chargement de registre à mémoire
(defun vm_store (vm src dest)
  (if (not (is_register_? src))
    (warn "ERR : <src> doit être un registre")
    (if (not (constantp dest))
      (warn "ERR : <dest> doit être une adresse mémoire (int)")
      (if (< (- (get vm :memory_size) 1) dest)
        (warn (concatenate 'string "ERR : <dest> l'adresse mémoire @" (write-to-string dest) " est hors limites [0 , " (write-to-string (- (get vm :memory_size) 1)) "]"))
        (let ((oldV (get-register vm src))
              (getDest (svref (get vm :memory) dest)))
          (setf (aref (get vm :memory) dest) (get-register vm src)))))))
;======================================================  

;======================================================  
; (MOVE <src> <dest>) = mouvement de registre à registre
(defun vm_move (vm src dest)
  (if (not (is_register_? dest))
    (warn "ERR : <dest> doit être un registre")
    (if (not (or (constantp src) (is_register_? src)))
      (warn "ERR : <src> doit être une constante ou un registre")
      (if (constantp src)
        (set-register vm src dest)
        (set-register vm (get-register vm src) dest)))))
;======================================================

; (ADD <src> <dest>)  = addition
(defun vm_add (vm src dest)
  )
; (SUB <src> <dest>)  = soustraction
(defun vm_sub (vm src dest)
  )
; (MUL <src> <dest>)  = multiplication
(defun vm_mul (vm src dest)
  )
; (DIV <src> <dest>)  = division
(defun vm_div (vm src dest)
  )
; (INCR <dest>)     = incrément
(defun vm_incr (vm dest)
  )
; (DECR <dest>)     = décrément
(defun vm_decr (vm dest)
  )
; (PUSH <src>)      = empiler
(defun vm_push (vm src)
  )
; (POP <dest>)      = dépiler
(defun vm_pop (vm dest)
  )
; (LABEL <label>)   = déclaration d’étiquette
(defun vm_label (vm label)
  )
; (JMP <label>)     = saut inconditionnel à une étiquette
(defun vm_jmp (vm label)
  )
; (JSR <label>)     = saut avec retour
(defun vm_jsr (vm label)
  )
; (RTN)         = retour
(defun vm_rtn (vm)
  )
; (CMP <src1> <src2>) = comparaison
(defun vm_cmp (vm src1 src2)
  )
; (JGT <label>)     = saut si plus grand
(defun vm_jgt (vm label)
  )
; (JGE <label>)     = saut si plus grand ou égal
(defun vm_jge (vm label)
  )
; (JLT <label>)     = saut si plus petit
(defun vm_jlt (vm label)
  )
; (JLE <label>)     = saut si plus petit ou égal
(defun vm_jle (vm label)
  )
; (JEQ <label>)     = saut si égal
(defun vm_jeq (vm label)
  )
; (JNE <label>)     = saut si différent
(defun vm_jne (vm label)
  )
; (TEST <src>)      = comparaison à NIL
(defun vm_test (vm src)
  )
; (JTRUE <label>)   = saut si non-NIL
(defun vm_jtrue (vm label)
  )
; (JNIL <label>)      = saut si NIL
(defun vm_jnil (vm label)
  )
; (NOP)         = rien
(defun vm_nop (vm)
  )
; (HALT)        = arrêt
(defun vm_halt (vm)
  )