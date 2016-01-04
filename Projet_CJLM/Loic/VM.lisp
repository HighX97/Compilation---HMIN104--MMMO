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
;
;
;
;
;La machine virtuelle (VM) dispose d’une mémoire (finie, mais suffisamment grande pour exécuter sans
;problème les programmes cible) et de trois registres généraux R0 , R1 et R2 . La mémoire est un ensemble
;fini de N cellules (ou mots). La première cellule a par définition le numéro 0, la dernière le numéro N − 1.
;On parlera plutôt d’adresse mémoire que de numéro de cellule. Afin de coller à une réalité simplifiée, nous
;distinguons d’une part le microprocesseur qui va interpréter le code et, d’autre part, la mémoire.
;Outre les 3 registres généraux, la machine possède plusieurs registres dédiés. La pile d’exécution est
;gérée dans la mémoire. La pile commence à l’adresse indiquée dans le registre spécial BP (base pointer).
;A priori, on ne touchera jamais à ce registre qui est initialisé une fois pour toute au démarrage de la MV.
;Le sommet de pile est indiqué dans le registre spécial SP (stack pointer). La pile est vide quand SP = BP .
;On a toujours SP ≥ BP (notre pile est montante). Le compteur de programme (ou compteur ordinal) est
;contenu dans un registre spécial PC (program counter).
;Dans la mémoire, une partie est alloué au stockage des programmes en cours d’exécution, une partie
;est allouée à la pile d’exécution, une partie au tas qui contient des structures de données. Enfin, une petite
;partie (dans la même zone que les programmes) aux variables globales du système.
;Pour l’instant, nous avons donc les registres suivants : R0 , R1 , R2 , BP , SP , PC . Nous introduirons dans
;la suite, un pointeur de cadre (frame pointer), FP , qui sert à définir des blocs de pile pour la structurer et
;faciliter les accès. Pour les comparaisons et les sauts conditionnels, on dispose de trois registres booléens
;(1 bit) appelés drapeaux (flag) : FLT , FEQ , FGT (pour drapeau plus petit, drapeau égal, drapeau plus grand).

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
(setq liste_registre '(R0 R1 R2 SP BP FP PC))
(setq liste_drapeau '(FLT FEQ FGT FNIL))
(setq mem_size_default 50)

;======================================================  
(defun is_register_? (registre)
  (position registre liste_registre))
;(trace is_register_?)
;======================================================

;======================================================  
;permet d'acceder à la valeur du registre
(defun vm_get_register (vm registre)
  (if (is_register_? registre)
    (get vm registre)
    (warn "ERR 1: doit etre un registre")))
;(trace vm_get_register)
;======================================================  

;======================================================  
;permet de changer la valeur du registre
(defun vm_set_register (vm registre valeur)
  (if (is_register_? registre)
    (setf (get vm registre) valeur)
    (warn "ERR 2: doit etre un registre")))
;(trace vm_set_register)  
;====================================================== 

;======================================================  
(defun is_flag_? (drapeau)
  (position drapeau liste_drapeau))
;(trace is_register_?)
;======================================================

;======================================================  
;permet d'acceder à la valeur du drapeau
(defun vm_get_flag (vm drapeau)
  (if (is_flag_? drapeau)
    (get vm drapeau)
    (warn "ERR 1: doit etre un drapeau")))
;(trace vm_get_flag)
;======================================================  

;======================================================  
;permet de changer la valeur du drapeau
(defun vm_set_flag_ON (vm drapeau)
  (if (is_flag_? drapeau)
    (setf (get vm drapeau) T)
    (warn "ERR 2: doit etre un drapeau")))
;(trace vm_set_flag_ON)  
;====================================================== 

;======================================================  
;permet de changer la valeur du drapeau
(defun vm_set_flag_OFF (vm drapeau)
  (if (is_flag_? drapeau)
    (setf (get vm drapeau) nil)
    (warn "ERR 2: doit etre un drapeau")))
;(trace vm_set_flag_OFF)  
;======================================================   

;======================================================  
(defun vm_init (vm &optional size)
	(if (null size)
		(setf size mem_size_default))
    (vm_init_memory vm size)
  	;(setf (get vm :debCode) (- size (* (floor (/ size 3)) 2)));depend de size 
  	;(setf (get vm :adrCode) (get vm :debCode));adresse pour charger dans le code
  	;(setf (get vm :ON) 0);interrupteur de la VM  on / off
    ;(setf (get vm :memory_size) size)
    (vm_set_register vm 'R0 nil)
    (vm_set_register vm 'R1 nil)
    (vm_set_register vm 'R2 nil)
    (vm_set_register vm 'SP 10);Plafond pile
    (vm_set_register vm 'BP 10);Plancher pile
    (vm_set_register vm 'FP 10);Pointeur de cadre
    (vm_set_register vm 'PC 10);Compteur ordinal
    (vm_set_flag_OFF vm 'FLT);drapeau <
    (vm_set_flag_OFF vm 'FEQ);drapeau =
    (vm_set_flag_OFF vm 'FGT);drapeau >
    (vm_set_flag_OFF vm 'FNIL);drapeau test
    ;(vm_set_register vm 'CO (- size (* (floor (/ size 3)) 2)))
  	(vm_init_hashTab_etq vm)) ;table pour les ref en avance
  	;(setf (get vm 'TSR) (make-hash-table))
;(trace vm_init)
;======================================================  

;======================================================  
(defun vm_state (vm)
  (and (print "=====Registres=====")
    (print "===Registres généraux===")
    (print (concatenate 'string "RO : " (write-to-string (vm_get_register vm 'R0))))
    (print (concatenate 'string "R1 : " (write-to-string (vm_get_register vm 'R1))))
    (print (concatenate 'string "R2 : " (write-to-string (vm_get_register vm 'R2))))
    (print "===Registres dédiés===")
    (print (concatenate 'string "SP : " (write-to-string (vm_get_register vm 'SP))))
    (print (concatenate 'string "BP : " (write-to-string (vm_get_register vm 'BP))))
    (print (concatenate 'string "FP : " (write-to-string (vm_get_register vm 'FP))))
    (print (concatenate 'string "PC : " (write-to-string (vm_get_register vm 'PC))))
    (print "=====Drapeaux=====")
    (print (concatenate 'string "FLT : " (write-to-string (vm_get_flag vm 'FLT))))
    (print (concatenate 'string "FEQ : " (write-to-string (vm_get_flag vm 'FEQ))))
    (print (concatenate 'string "FGT : " (write-to-string (vm_get_flag vm 'FGT))))
    (print (concatenate 'string "FNIL : " (write-to-string (vm_get_flag vm 'FNIL))))
    (print "=====Memory=====")
    (print (concatenate 'string "memory : (vm_state_memory '" (write-to-string vm) ")"))
    (print "=====HashTab_etq=====")
    (print (concatenate 'string "hashTab_etq : (vm_state_hashTab_etq '" (write-to-string vm) ")"))
    (print "")))
;(trace vm_state) 
;======================================================  

;======================================================  
(defun vm_init_memory (vm size)
  (setf (get vm :memory) (make-array size)))
;(trace vm_init_memory)
;======================================================  

;======================================================  
(defun vm_get_memory (vm)
  (get vm :memory))
;(trace vm_get_memory)
;======================================================  

;======================================================  
(defun vm_set_memory (vm dest)
  (warn "To Do"))
;(trace vm_set_memory)
;======================================================  

;======================================================  
(defun vm_state_memory (vm)
  (and 
    (print "memory : ")
    (vm_get_memory vm)))
;(trace vm_state_memory)
;======================================================  

;======================================================  
(defun vm_init_hashTab_etq (vm)
  (setf (get vm :hashTab_etq) (make-hash-table)))
;(trace vm_init_hashTab_etq)
;======================================================  

;======================================================  
(defun vm_get_hashTab_etq (vm)
  (get vm :hashTab_etq))
;(trace vm_get_hashTab_etq)
;======================================================  

;======================================================  
(defun vm_get_hashTab_etq_val (vm etq)
  (gethash etq (vm_get_hashTab_etq vm)))
;(trace vm_get_hashTab_etq_val)
;======================================================  

;======================================================  
(defun vm_set_hashTab_etq (vm etq valeur)
  (setf (gethash etq (vm_get_hashTab_etq vm)) valeur))
;(trace vm_set_hashTab_etq)
;======================================================  

;======================================================  
(defun vm_state_hashTab_etq (vm)
  (and 
    (print "hashTab_etq :")
    (vm_get_hashTab_etq vm)))
;(trace vm_state_hashTab_etq)
;======================================================   

;======================================================  
; (MOVE <src> <dest>) = mouvement de registre à registre
; (MOVE (:LIT . 50) R0)
; (MOVE R0 R1)

;(defun vm_move (vm src dest)
;  (if (not (is_register_? dest))
 ;   (warn "ERR : <dest> doit être un registre")
  ;  (if (not (or (constantp src) (is_register_? src)))
   ;   (warn "ERR : <src> doit être une constante ou un registre")
    ;  (if (constantp src)
     ;   (vm_set_register  vm dest src)
      ;  (vm_set_register  vm dest (vm_get_register vm src))))))

(defun vm_move (vm src dest)
  (if (not (is_register_? dest))
    (warn "ERR : <dest> doit être un registre")
    (if (atom src)
      (if (is_register_? src)
        (vm_set_register  vm dest (vm_get_register vm src))
        (warn "ERR : <src> doit être un registre ou (:LIT . n) avec n entier"))
      (if (and (eq (car src) ':LIT) (integerp (cdr src)))
        (vm_set_register  vm dest (cdr src))
        (warn "ERR : <src> doit être un registre ou (:LIT . n) avec n entier")))))
;(trace vm_move) 
;======================================================

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
          (vm_set_register  vm dest getSrc)))))))
;(trace vm_load) 
;======================================================  

;======================================================  
; (STORE <src> <dest> = chargement de registre à mémoire
  (defun vm_store (vm src dest)
    (if (not (is_register_? src))
      (warn "ERR : <src> doit être un registre")
      (if (not (or (constantp dest) (is_register_? dest)))
        (warn "ERR : <dest> doit être un registre ou une adresse mémoire (int)")
        (if (is_register_? dest)
          (setf (aref (get vm :memory) (vm_get_register vm dest) (vm_get_register vm src)))
        (if (< (- (get vm :memory_size) 1) dest)
          (warn (concatenate 'string "ERR : <dest> l'adresse mémoire @" (write-to-string dest) " est hors limites [0 , " (write-to-string (- (get vm :memory_size) 1)) "]"))
          (let ((oldV (vm_get_register vm src))
            (getDest (svref (get vm :memory) dest)))
          (setf (aref (get vm :memory) dest) (vm_get_register vm src))))))))
;(trace vm_store) 
;======================================================  

;======================================================  
; (ADD <src> <dest>)  = addition
(defun vm_add (vm src dest)
  (cond
    ((not (is_register_? dest))
      (warn "ERR : <dest> doit être un registre"))
    ((atom src)
      (if (not (is_register_? src))
        (warn "ERR : <src> doit être un registre ou '(:LIT . n)")
        (vm_set_registervm (+ (vm_get_register vm src) (vm_get_register vm dest)) dest)))
    ((not (atom src))
      (if (and (eq (car src) ':LIT) (integerp (cdr src)))
        (vm_set_registervm (+ (cdr src) (vm_get_register vm dest)) dest)
        (warn "ERR : <src> doit être un registre ou (:LIT n) avec n entier")))))
;(trace vm_add) 
;====================================================== 

;======================================================  
; (SUB <src> <dest>)  = soustraction
(defun vm_sub (vm src dest)
  (cond
    ((not (is_register_? dest))
      (warn "ERR : <dest> doit être un registre"))
    ((atom src)
      (if (not (is_register_? src))
        (warn "ERR : <src> doit être un registre ou '(:LIT . n)")
        (vm_set_registervm (- (vm_get_register vm dest) (vm_get_register vm src)) dest)))
    ((not (atom src))
      (if (and (eq (car src) ':LIT) (integerp (cdr src)))
        (vm_set_registervm (- (vm_get_register vm dest) (cdr src)) dest)
        (warn "ERR : <src> doit être un registre ou (:LIT n) avec n entier")))))
;(trace vm_sub) 
;====================================================== 

;======================================================  
; (MUL <src> <dest>)  = multiplication
(defun vm_mul (vm src dest)
  (cond
    ((not (is_register_? dest))
      (warn "ERR : <dest> doit être un registre"))
    ((atom src)
      (if (not (is_register_? src))
        (warn "ERR : <src> doit être un registre ou '(:LIT . n)")
        (vm_set_registervm (* (vm_get_register vm src) (vm_get_register vm dest)) dest)))
    ((not (atom src))
      (if (and (eq (car src) ':LIT) (integerp (cdr src)))
        (vm_set_registervm (* (cdr src) (vm_get_register vm dest)) dest)
        (warn "ERR : <src> doit être un registre ou (:LIT n) avec n entier")))))
;(trace vm_mul) 
;======================================================  

;====================================================== 
; (DIV <src> <dest>)  = division
(defun vm_div (vm src dest)
  (cond
    ((not (is_register_? dest))
      (warn "ERR : <dest> doit être un registre"))
    ((atom src)
      (if (not (is_register_? src))
        (warn "ERR : <src> doit être un registre ou '(:LIT . n)")
        (if (eq (vm_get_register vm src) 0)
          (warn "ERR : DIVISION PAR 0:<src> ")
          (vm_set_registervm (/ (vm_get_register vm dest) (vm_get_register vm src)) dest))))
    ((not (atom src))
      (if (and (eq (car src) ':LIT) (integerp (cdr src)))
        (if (eq 0 (cdr src))
          (warn "ERR : DIVISION PAR 0:<src> ")
          (vm_set_registervm (/ (vm_get_register vm dest) (cdr src)) dest))
        (warn "ERR : <src> doit être un registre ou (:LIT n) avec n entier")))))
;(trace vm_div) 
;====================================================== 

;======================================================  
; (INCR <dest>)     = incrément
(defun vm_incr (vm dest)
  (if (not (is_register_? dest))
    (warn "ERR3 : <dest> doit être un registre")
    (vm_set_register vm dest (+ (vm_get_register vm dest) 1))))
;(trace vm_incr) 
;====================================================== 

;======================================================  
; (DECR <dest>)     = décrément
(defun vm_decr (vm dest)
  (if (not (is_register_? dest))
    (warn "ERR4 : <dest> doit être un registre")
    (vm_set_register vm dest (- (vm_get_register vm dest) 1))))
;(trace vm_decr) 
;======================================================

;======================================================   
; (PUSH <src>)      = empiler
(defun vm_push (vm src)
  (if (atom src)
    (if (not (is_register_? src))
      (warn "ERR : <src> doit être un registre ou '(:LIT . n)")
      (progn (vm_incr vm 'SP)
        (vm_store vm src (vm_get_register vm 'SP))))
    (if (and (eq (car src) ':LIT) (integerp (cdr src)))
      (progn (vm_incr vm 'SP)
        (vm_move vm src 'R0)
        (vm_store vm 'R0 (vm_get_register vm 'SP)))
      (warn "ERR : <src> doit être un registre ou (:LIT n) avec n entier"))))
;(trace vm_push) 
;======================================================

;======================================================   
; (POP <dest>)      = dépiler
(defun vm_pop (vm dest)
  (if (not (is_register_? dest))
    (warn "ERR : <dest> doit être un registre ou '(:LIT . n)")
    (and (vm_load vm (vm_get_register vm 'SP) dest)
      (vm_decr vm 'SP))))
;(trace vm_pop) 
;======================================================  

;======================================================
;Doit-on supprimer une étiquette après utilisation ?
;Peut-on décclaré deux fois la même étiquette ? 
; (LABEL <label>)   = déclaration d’étiquette
(defun vm_label (vm label)
  (vm_set_hashTab_etq vm label (vm_get_register vm 'SP)))
;(trace vm_label) 
;======================================================

;====================================================== 
; (JMP <label>)     = saut inconditionnel à une étiquette ou une adresse
(defun vm_jmp (vm label)
  (if (integerp label)
    (vm_move vm (cons :lit label) 'PC)
    (vm_move vm (cons :lit (vm_get_hashTab_etq_val vm label)) 'PC)))
;(trace vm_jmp) 
;====================================================== 

;====================================================== 
; (JSR <label>)     = saut avec retour
(defun vm_jsr (vm label)
  (and (vm_push vm 'PC)
    (vm_jmp vm label)))
;(trace vm_jsr) 
;====================================================== 

;====================================================== 
; (RTN)         = retour
(defun vm_rtn (vm)
  (and (vm_move vm (cons :lit (vm_get_register vm 'SP)) 'R0)
    (vm_decr vm 'SP)
    (vm_jmp vm (vm_get_register vm 'RO))))
;(trace vm_rtn) 
;====================================================== 

(defun vm_test_constante (src)
  (and (eq (car src) ':LIT) (integerp (cdr src))))
;====================================================== 
; (CMP <src1> <src2>) = comparaison
(defun vm_cmp (vm src1 src2)
  (if (and (vm_test_constante src1) (vm_test_constante src2))
    (cond
      ((< (cdr src1) (cdr src2))
        (vm_set_flag_ON vm 'FLT)
        (vm_set_flag_OFF vm 'FEQ)
        (vm_set_flag_OFF vm 'FGT))
      ((= (cdr src1) (cdr src2))
        (vm_set_flag_OFF vm 'FLT)
        (vm_set_flag_ON vm 'FEQ)
        (vm_set_flag_OFF vm 'FGT))
      ((> (cdr src1) (cdr src2))
        (vm_set_flag_OFF vm 'FLT)
        (vm_set_flag_OFF vm 'FEQ)
        (vm_set_flag_ON vm 'FGT)))))    
;(trace vm_cmp) 
;====================================================== 

;====================================================== 
; (JGT <label>)     = saut si plus grand
(defun vm_jgt (vm label)
  (if (and  (not (vm_get_flag vm FLT)) 
            (not (vm_get_flag vm FEQ)) 
            (vm_get_flag vm FGT))
    (vm_jmp vm label)
    (vm_incr vm 'PC)))
(trace vm_jgt) 
;====================================================== 

;====================================================== 
; (JGE <label>)     = saut si plus grand ou égal
(defun vm_jge (vm label)
  (if (and  (not (vm_get_flag vm FLT)) 
            (or (vm_get_flag vm FEQ) 
                (vm_get_flag vm FGT)))
    (vm_jmp vm label)
    (vm_incr vm 'PC)))
(trace vm_jge) 
;====================================================== 

;====================================================== 
; (JLT <label>)     = saut si plus petit
(defun vm_jlt (vm label)
  (if (and  (vm_get_flag vm FLT) 
            (not (vm_get_flag vm FEQ)) 
            (not (vm_get_flag vm FGT)))
    (vm_jmp vm label)
    (vm_incr vm 'PC)))
(trace vm_jlt) 
;====================================================== 

;====================================================== 
; (JLE <label>)     = saut si plus petit ou égal
(defun vm_jle (vm label)
  (if (and  (or (vm_get_flag vm FLT) 
                (vm_get_flag vm FEQ)) 
            (not (vm_get_flag vm FGT)))
    (vm_jmp vm label)
    (vm_incr vm 'PC)))
(trace vm_jle) 
;====================================================== 

;====================================================== 
; (JEQ <label>)     = saut si égal
(defun vm_jeq (vm label)
  (if (and  (not (vm_get_flag vm FLT)) 
            (vm_get_flag vm FEQ) 
            (not (vm_get_flag vm FGT)))
    (vm_jmp vm label)
    (vm_incr vm 'PC)))
(trace vm_jeq) 
;====================================================== 

;====================================================== 
; (JNE <label>)     = saut si différent
(defun vm_jne (vm label)
  (if (and  (vm_get_flag vm FLT) 
            (not (vm_get_flag vm FEQ)) 
            (vm_get_flag vm FGT))
    (vm_jmp vm label)
    (vm_incr vm 'PC)))
(trace vm_jne) 
;====================================================== 

;====================================================== 
; (TEST <src>)      = comparaison à NIL
(defun vm_test (vm src)
  (if src
    vm_set_flag_ON 'FNIL
    vm_set_flag_OFF 'FNIL))
(trace vm_test) 
;====================================================== 

;====================================================== 
; (JTRUE <label>)   = saut si non-NIL
(defun vm_jtrue (vm label)
  (if (vm_get_flag vm FNIL)
    (vm_jmp vm label)
    (vm_incr vm 'PC)))
(trace vm_jtrue) 
;====================================================== 

;====================================================== 
; (JNIL <label>)      = saut si NIL
(defun vm_jnil (vm label)
  (if (not (vm_get_flag vm FNIL))
    (vm_jmp vm label)
    (vm_incr vm 'PC)))
(trace vm_jnil) 
;====================================================== 

;====================================================== TO DO
; (NOP)         = rien
(defun vm_nop (vm)
  )
(trace vm_nop) 
;====================================================== 

;====================================================== TO DO
; (HALT)        = arrêt
(defun vm_halt (vm)
  )
;(trace vm_halt)
;======================================================  

;====================================================== TO DO
; (HALT)        = arrêt
(defun vm_read_asm (vm asm)
  (if (atom asm)
    nil
    (let ((fun (caar asm)) 
      (args (cdar asm))
      (rest (cdr asm)))
    ;
    (cond
      ((eq fun 'LOAD)
        (progn
          (vm_load vm (first args) (second args))
          (vm_read_asm vm rest)))
      ((eq fun 'STORE)
        (progn
          (vm_store vm (first args) (second args))
          (vm_read_asm vm rest)))
      ((eq fun 'MOVE)
        (progn
          (vm_move vm (first args) (second args))
          (vm_read_asm vm rest)))
      ((eq fun 'ADD)
        (progn
          (vm_add vm (first args) (second args))
          (vm_read_asm vm rest)))
      ((eq fun 'SUB)
        (progn
          (vm_sub vm (first args) (second args))
          (vm_read_asm vm rest)))
      ((eq fun 'MUL)
        (progn
          (vm_mul vm (first args) (second args))
          (vm_read_asm vm rest)))
      ((eq fun 'DIV)
        (progn
          (vm_div vm (first args) (second args))
          (vm_read_asm vm rest)))
      ((eq fun 'INCR)
        (progn
          (vm_incr vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'DECR)
        (progn
          (vm_decr vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'PUSH)
        (progn
          (vm_push vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'POP)
        (progn
          (vm_pop vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'LABEL)
        (progn
          (vm_label vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'JMP)
        (progn
          (vm_jmp vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'JSR)
        (progn
          (vm_jsr vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'RTN)
        (progn
          (vm_rtn vm)
          (vm_read_asm vm rest)))
      ((eq fun 'CMP)
        (progn
          (vm_cmp vm (first args) (second args))
          (vm_read_asm vm rest)))
      ((eq fun 'JGT)
        (progn
          (vm_jgt vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'JGE)
        (progn
          (vm_jge vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'JLT)
        (progn
          (vm_jlt vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'JLE)
        (progn
          (vm_jle vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'JEQ)
        (progn
          (vm_jeq vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'JNE)
        (progn
          (vm_jne vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'TEST)
        (progn
          (vm_test vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'JTRUE)
        (progn
          (vm_jtrue vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'JNIL)
        (progn
          (vm_jnil vm (first args))
          (vm_read_asm vm rest)))
      ((eq fun 'NOP)
        (progn
          (vm_nop vm)
          (vm_read_asm vm rest)))
      ((eq fun 'HALT)
        (progn
          (vm_halt vm)
          (vm_read_asm vm rest)))))))
;(trace vm_halt)
;======================================================  