;;;; Created on 2012-11-21 10:07:28

(require "litoasm.lisp")

;Machine virtuelle a pile :)
;tmtc
;(load "C:/Users/Marama/workspace/IN108Project/makevm.lisp")
;(load "/auto_home/dvaret/workspace/LispProjet/makevm.lisp")
;(load-code 'vm '((:STACK 0) (:CONST 2) (:CONST 1) (:CALL +)))
;(comp '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1))))))
;(comp '(defun fibo (n) (if (<= n 1) 1 (+ (fibo (- n 1)) (fibo (- n 2))))))
;(comp '(defun t0 (n) (if (<= n 0) (+ n n) (* (t0 (- n 1)) n))))
;(vmeval 'vm '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1))))))
;(vmeval 'vm '(defun fibo (n) (if (<= n 1) 1 (+ (fibo (- n 1)) (fibo (- n 2))))))
;(vmeval 'vm '(defun test (n) (if (<= n 0) (+ n n) (* n n))))
;(vmeval 'vm '(defun fibo2 (n x y) (if (<= n 0) y (fibo2 (- n 1) y (+ x y)))))
;(vmeval 'vm '(defun fiboterm (n) (fibo2 n 1 0)))
; x = 1, x = 0 

(defun makevm (vm &optional (taille ()))
  (if (not (null taille))
    (set-pile vm taille)
    (set-pile vm)))

(defun set-pile (vm &optional (taille ()))
  (cond
    ((not (null taille))
      (setf (get vm 'pile) (make-array taille :initial-element 0))
      (set-case vm 3 (- taille 1)) ;PC
      (set-case vm 4 (- taille 1)) ;CC (code counter)
      )
    (T
      (setf (get vm 'pile) (make-array 1000 :initial-element 0))
      (set-case vm 3 999) ;PC
      (set-case vm 4 999) ;CC (code counter)
      ))
  ;(setf (get vm 'pile) (make-array 1000 :initial-element 0))
  (set-case vm 0 8) ;BP
  (set-case vm 1 8) ;FP
  (set-case vm 2 8) ;SP
  ;(set-case vm 3 999) ;PC
  ;(set-case vm 4 999) ;CC (code counter)
  ;Flags non utilises en realite. :(
  (set-case vm 5 0) ;FLT
  (set-case vm 6 0) ;FEQ
  (set-case vm 7 0) ;FGT
  
  (setf (get vm 'etiquettes_resolues) (make-hash-table))
  (setf (get vm 'references_avant) (make-hash-table))
  vm
  )

(defun resetvm (vm)
  (makevm vm))

(defun resetpile (vm)
  (set-case vm 1 8)
  (set-case vm 2 8))


;permet d'evaluer deux expressions a la fois
;par exemple : definition de fonction et appel
;(vmeval 'nom_de_la_vm '(expression) '(definition associee a l'expression ou seconde expression))
(defun vmeval (vm code &optional (code2 ()))
  ;code optionnel (ex : definition de fonction)
  (if (not (null code2))
    (if (eq (first code2) 'defun)
    (progn
      ;(load_vm vm (comp code2))
      (charge_vm vm (comp code2))
      (set-case vm 3 (get-cc vm))
      (second code2))
    (progn
      ;(load_vm vm (comp code))
      (charge_vm vm (comp code2))
      (read-code vm)
      (print (get-SP-value vm)))))
  ;code
  (if (eq (first code) 'defun)
    (progn
      ;(load_vm vm (comp code2))
      (charge_vm vm (comp code))
      (set-case vm 3 (get-cc vm))
      (second code))
    (progn
      ;(load_vm vm (comp code))
      (charge_vm vm (comp code))
      (read-code vm)
      (get-SP-value vm))))

(defun charge_vm (vm code)
  (let ((ghtrr (get vm 'etiquettes_resolues))
         (ghtra (get vm 'references_avant))
         (lhtrr (make-hash-table))
         (lhtra (make-hash-table)))
    (charge_vm_acc vm code ghtrr ghtra lhtrr lhtra)))

(defun charge_vm_acc (vm code ghtrr ghtra lhtrr lhtra)
  (if (atom code)
      ()
      (let ((instr (first code)))
      (cond
          ((eq (first instr) :label)
            (let ((addr (get-CC vm)) (label (second instr)))
              (if (symbolp (second instr))
                  (progn
                    (load-label addr label ghtrr ghtra)
                    (charge_vm_acc vm (cdr code) ghtrr ghtra lhtrr lhtra))
                  (progn
                    (load-label addr label lhtrr lhtra)
                    (charge_vm_acc vm (cdr code) ghtrr ghtra lhtrr lhtra))
                    )))
          ;((eq (first instr) :skip)
           ; (let ((addr (get-CC vm)) (label (second instr)))
            ;  (if (symbolp (second instr))
             ;   (load-jmp vm addr label ghtrr ghtra)
              ;  (load-jmp vm addr label lhtrr lhtra))))
        (T
           (set-case vm (get-CC vm) instr)
           (vmdecr vm 4)
           (charge_vm_acc vm (cdr code) ghtrr ghtra lhtrr lhtra)
           )))))

(defun load_vm (vm code)
  (let ((ghtrr (get vm 'etiquettes_resolues))
         (ghtra (get vm 'references_avant))
         (lhtrr (make-hash-table))
         (lhtra (make-hash-table)))
    (loop for instr in code
      do
        (cond
          ((eq (first instr) :label)
            (let ((addr (get-CC vm)) (label (second instr)))
              (if (symbolp (second instr))
                (load-label addr label ghtrr ghtra)
                (load-label addr label lhtrr lhtra))))
          ;((eq (first instr) :skip)
           ; (let ((addr (get-CC vm)) (label (second instr)))
            ;  (if (symbolp (second instr))
             ;   (load-jmp vm addr label ghtrr ghtra)
              ;  (load-jmp vm addr label lhtrr lhtra))))
          (T
            (set-case vm (get-CC vm) instr)
            (vmdecr vm 4)
            )))))
         

(defun load-label (addr label htrr htra)
  (let ((etat T))
    (if (gethash label htrr)
        (progn
          (setf etat NIL)
          (warn "Etiquette deja declaree"))
        (progn
          (setf (gethash label htrr) addr)
          (if (gethash label htra)
            ()
            ()
            )))
    etat))

(defun load-jmp (vm addr label htrr htra)
  ())

(defun load-code (vm code)
  (let ((index (get-CC vm)))
    (if
        (atom code)
        ()
        (progn 
          (set-case vm index (car code))
          (vmdecr vm 4)
          (load-code vm (cdr code))
          )
        ))
  (values))


(defun read-code (vm)
  ;if atom ? if = 0 ? --> stop
  ;(loop for x from (get-PC vm) downto (+ 1 (get-CC vm))
  (loop while (not (eq (get-case vm (get-PC vm)) 0))
    do
    (let ((instr (get-case vm (get-PC vm)))); (adret 0) (x (read)))
      ;(showvm vm)
      ;(print (first instr))
      (if (atom instr)
        (return)
        (cond
          ((eq (first instr) :stack)
            (vmstack vm (second instr))
            (vmdecr vm 3)
            )
          ((eq (first instr) :call)
            ;(setf adret (- (get-PC vm) 1))
            (vmcall vm (second instr))
            (vmdecr vm 3)
            )
          ((eq (first instr) :const)
            (vmconst vm (second instr))
            (vmdecr vm 3)
            )
          ((eq (first instr) :var)
            (vmvar vm (second instr))
            (vmdecr vm 3)
            )
          ((eq (first instr) :set-var)
            (vmset-var vm) ;(second instr))
            (vmdecr vm 3)
            )
          ((eq (first instr) :rtn)
            (vmrtn vm)
            )
          ;;;; !!!!
          ((eq (first instr) :skip)
            (vmskip vm (second instr))
            (vmdecr vm 3)
            )
          ((eq (first instr) :skipnil)
            (vmskipnil vm (second instr))
            (vmdecr vm 3)
            )
          ((eq (first instr) :skiptrue)
            (vmskiptrue vm (second instr))
            (vmdecr vm 3)
            )
          ((eq (first instr) :jump)
            (vmjump vm (second instr))
            (vmdecr vm 3) ; ?
            )
          ((eq (first instr) :load)
            (vmload vm (second instr))
            (vmdecr vm 3)
            )
          ((eq (first instr) :store)
            (vmstore vm (second instr))
            (vmdecr vm 3)
            )))))
  (get-SP-value vm))



(defun showvm (vm)
  (print 'COUNTERS)
  (print (list :BP (get-case vm 0)))
  (print (list :FP (get-case vm 1)))
  (print (list :SP (get-case vm 2)))
  (print (list :PC (get-case vm 3)))
  (print (list :CC (get-case vm 4)))
  (print 'FLAGS)
  (print (list :FLT (get-case vm 5)))
  (print (list :FEQ (get-case vm 6)))	
  (print (list :FGT (get-case vm 7)))	
  (print 'PILE)	
  (print (get-pile vm))
  (values)	
  )	

;renvoie le tableau contenant la pile
(defun get-pile (vm)	
  (get vm 'pile))	

;renvoie la valeur situee a la position index
(defun get-case (vm index)	
  (aref (get-pile vm) index))

;remplace la valeur situee a la position index par val
(defun set-case (vm index val)	
  (setf (aref (get-pile vm) index) val))

;revoie le BP
(defun get-BP (vm)
  (aref (get vm 'pile) 0))

;renvoie le FP
(defun get-FP (vm)
  (aref (get vm 'pile) 1))

;renvoie le SP
(defun get-SP (vm)
  (aref (get vm 'pile) 2))

;revoie le PC
(defun get-PC (vm)
  (aref (get vm 'pile) 3))

;renvoie le CC
(defun get-CC (vm)
  (aref (get vm 'pile) 4))

;modifie la valeur du SP
(defun set-SP (vm val)	
  (setf (aref (get vm 'pile) 2) val))

;modifie la valeur du FP
(defun set-FP (vm val)
  (setf (aref (get vm 'pile) 1) val))

;renvoie la valeur du SP +/- index
(defun SP (vm op index)
  (cond	
    ((eq '- op)
     (- (get-SP vm) index))
    ((eq '+ op) 	
     (+ (get-SP vm) index))))

;renvoie la valeur du FP +/- index
(defun FP (vm op index)
  (cond	
    ((eq '- op)
     (- (get-FP vm) index))
    ((eq '+ op) 	
     (+ (get-FP vm) index))))

(defun get-SP-value (vm)
  (aref (get vm 'pile) (- (get-SP vm) 1)))

(defun get-FP-value (vm)
  (get-case vm (get-FP vm)))
  

;push/pop
(defun vmpush (vm val)
  (set-case vm (get-SP vm) val)
  (vmincr vm 2)	
  )	

(defun vmpop (vm)
  (progn	
    (vmdecr vm 2)
    (get-case vm (get-SP vm)))
  )	

(defun vmconst (vm lit)
  ;(print :const)
  (vmpush vm lit)	
  )	

(defun vmvar (vm n)
  ;(print :var)
  ;(print (get-FP-value vm))
  ;(print (+ 1 (- (get-FP-value vm) n)))
  ;(print (get-case vm (FP vm '- (+ 1 (- (get-FP-value vm) n))))) ;omg
  (vmpush vm (get-case vm (FP vm '- (+ 1 (- (get-FP-value vm) n))))))

(defun vmset-var (vm)
  ())

(defun vmstack (vm n)
  ;(print :stack)
  (vmpush vm (+ 2 n)) ;nombre de parametre + 3 (oSP + oFP + @ ret)
  (set-case vm 1 (- (get-SP vm) 1))) ;le FP recoit le SP
  ;(set-case vm 1 (+ 1 (get-FP vm))))
  ;(set-case vm (get-SP vm) (- (get-SP vm) n))
  ;(vmincr vm 2))

;analyse par cas :)
;cas : 	
; - operation arithmetique -> appel aux fonctions definies par la vm
; - formes speciales / operateurs lisp ->
; - fonctions definies par l'user -> recherche par etiquette
(defun vmcall (vm f)
  ;(print :call)	
  (cond
    ((is-operator? f) (operator vm f))
    ((is-lisp-form? vm f) (lisp-form vm f))
    ((is-user-defined? vm f) (user-defined vm f))
    ))	

;(defun vmrtn (vm)
 ; (if (<= (get-SP vm) 8)
  ;    (vmdecr vm 3)
   ;   (progn
    ;    (vmdecr vm 2)
     ;   (vmdecr vm 3))))

(defun vmrtn (vm)
  ;(print :rtn)
  (let ((val (vmpop vm)) (nbpar (vmpop vm)) (adret (vmpop vm)) (osp (vmpop vm)));(ofp (vmpop vm)) (osp (vmpop vm)))
    ;(print :return)
    ;(print val)
    ;(print nbpar)
    ;(print adret)
    ;(print osp)
    (set-fp vm (- osp 1))
    (set-sp vm (- osp (- nbpar 3)))
    (vmpush vm val)
    ;(set-case vm (get-SP vm) val)
    (set-case vm 3 (- adret 1))))
  ;(print :vmrtn)
  ;(showvm vm))
  

(defun vmskip (vm n)
  ;(print :skip)
  (set-case vm 3 (- (get-PC vm) n)))

(defun vmskipnil (vm n)
  ;(print :skipnil)
  ;(print "---SP---Value---")
  ;(print (get-SP-value vm))
  (let ((val (vmpop vm)))
    ;(print val)
    (if (eq nil val) ;(get-SP-value vm))
      (vmskip vm n)
      ()
      )))

(defun vmskiptrue (vm n)
  ;(print "---SP---Value---")
  ;(print (get-SP-value vm))
  (let ((val (vmpop vm)))
  (if (eq T val)
    (vmskip vm n)
    ()
    )))

(defun vmjump (vm n)
  ;test ?
  (set-case vm 3 (+ 1 (gethash fun (get vm 'etiquettes_resolues)))))

(defun vmload (vm n)
  ())	

;bricolage :(
(defun vmstore (vm n)
  (let ((val (vmpop vm)) (nbparam (vmpop vm)) (adret (vmpop vm)) (osp (vmpop vm)))
    (vmincr vm 1)
    (vmpush vm val)
    (vmpush vm (+ 1 osp))
    (vmpush vm adret)
    (vmpush vm (+ 1 nbparam))))	

;;Instructions arithmetiques
;operateurs definis : +, -, *, /, ++, --, <, >, =
(defun is-operator? (fun)	
  (or
    (eq '+ fun)
    (eq '- fun)
    (eq '* fun)
    (eq '/ fun)
    (eq '++ fun)
    (eq '-- fun)
    (eq '> fun)
    (eq '< fun)
    (eq '= fun)
    (eq '<= fun)
    (eq '>= fun)))

(defun operator (vm fun)
  ;(print fun)
  (let ((src (SP vm '- 1)) (dest (SP vm '- 2)))
    (cond	
      ((eq '+ fun)
       (vmadd vm src dest))
      ((eq '- fun)	
       (vmsub vm src dest))
      ((eq '* fun)
       (vmmul vm src dest))
      ((eq '/ fun)
       (vmdiv vm src dest))
      ((eq '++ fun)
       (vmincr vm src))
      ((eq '-- fun)
       (vmdecr vm src))
      ((eq '= fun)
       (vmcmp vm '= src dest))
      ((eq '> fun)
       (vmcmp vm '> src dest))
      ((eq '< fun)
       (vmcmp vm '< src dest))
      ((eq '<= fun)
       (vmcmp vm '<= src dest))
      ((eq '>= fun)
       (vmcmp vm '>= src dest))
      )))


;addition
;(defun vmadd (vm src dest)
 ; (let ((v2 (vmpop vm)) (v1 (get-case 'vm dest)));(v1 (vmpop vm)))
  ;  (set-case vm dest (+ v1 v2))))

(defun vmadd (vm src dest)
  (let ((v2 (vmpop vm)) (v1 (vmpop vm)))
    (vmpush vm (+ v1 v2))))

;soustraction
;(defun vmsub (vm src dest)
 ; (let ((v2 (vmpop vm)) (v1 (get-case 'vm dest)));(v1 (vmpop vm)))
  ;  (set-case vm dest (- v1 v2))))

(defun vmsub (vm src dest)
  (let ((v2 (vmpop vm)) (v1 (vmpop vm)))
    (vmpush vm (- v1 v2))))

;multiplication
;(defun vmmul (vm src dest)
 ; (let ((v2 (vmpop vm)) (v1 (get-case 'vm dest)));(v1 (vmpop vm)))
  ;  (set-case vm dest (* v1 v2))))

(defun vmmul (vm src dest)
  (let ((v2 (vmpop vm)) (v1 (vmpop vm)))
    (vmpush vm (* v1 v2))))

;division
;(defun vmdiv (vm src dest)
 ; (let ((v2 (vmpop vm)) (v1 (get-case 'vm dest)));(v1 (vmpop vm)))
  ;  (set-case vm dest (/ v1 v2))))

(defun vmdiv (vm src dest)
  (let ((v2 (vmpop vm)) (v1 (vmpop vm)))
    (vmpush vm (/ v1 v2))))

;increment
(defun vmincr (vm dest)
  (let ((v1 (get-case vm dest)))
    (set-case vm dest (+ v1 1))))
;decrement
(defun vmdecr (vm dest)
  (let ((v1 (get-case vm dest)))
    (set-case vm dest (- v1 1))))

;cmp - flags
;(defun vmcmp (vm src1 src2)
; (reset-flags vm)
;(cond 
; ((< (get vm src1) (get vm src2))
; (vmincr vm 'FLT))
;((> (get vm src1) (get vm src2))
;(vmincr vm 'FGT))
;((eq (get vm src1) (get vm src2))
;(vmincr vm 'FEG)))
;(show-flags vm)
;)

(defun vmcmp (vm op src dest)
  ;(print op)
  (let ((v2 (vmpop vm)) (v1 (vmpop vm)))
    (case op
      (=
        (vmpush vm (= v1 v2)))
        ;(set-case vm dest (= v1 v2)))
      (>
        (vmpush vm (> v1 v2)))
        ;(set-case vm dest (> v1 v2)))
      (<
        (vmpush vm (< v1 v2)))
        ;(set-case vm dest (< v1 v2)))
      (<=
        (vmpush vm (<= v1 v2)))
       	;(set-case vm dest (<= v1 v2)))
      (>=
        (vmpush vm (>= v1 v2)))
        ;(set-case vm dest (>= v1 v2)))
      )
    ))
;raz des flags
(defun reset-flags (vm)
  (set-case vm 3 0) ;FLT
  (set-case vm 4 0) ;FEQ
  (set-case vm 5 0) ;FGT
  )

(defun is-lisp-form? (vm fun)
  ;(print :lisp-form)
  ;(print fun)
  ;a modifier pour effectuer l'appel en fonction du nombre de parametres de la fonction
  ;apply fun apres vmpop
  (case fun
    ('print
      (print (vmpop vm)))
    ('get
      (let ((arg2 (vmpop vm)) (arg1 (vmpop vm)))
        (vmpush vm (get arg1 arg2))))
    ('make-hash-table
      (vmpush vm (make-hash-table)))
    ('atom
      (vmpush vm (atom (vmpop vm))))
    ('first
      (vmpush vm (first (vmpop vm))))
    ('second
      (vmpush vm (second (vmpop vm))))
    ('cdr
      (vmpush vm (cdr (vmpop vm))))
    ('eq
      (vmpush vm (eq (vmpop vm) (vmpop vm))))
    ('symbolp
      (vmpush vm (symbolp (vmpop vm))))
    ('aref
      (let ((arg2 (vmpop vm)) (arg1 (vmpop vm)))
        (vmpush vm (aref arg1 arg2))))
    ('get-cc
      (vmpush vm (get-cc (vmpop vm))))
    ('set-case
      (let ((arg3 (vmpop vm)) (arg2 (vmpop vm)) (arg1 (vmpop vm)))
        (vmpush vm (set-case arg1 arg2 arg3))))
    ('get-pile
      (vmpush vm (get-pile (vmpop vm))))
    ('vmdecr
      (let ((arg2 (vmpop vm)) (arg1 (vmpop vm)))
        (vmpush vm (vmdecr arg1 arg2))))
    ('get-case
      (let ((arg2 (vmpop vm)) (arg1 (vmpop vm)))
      (vmpush vm (get-case arg1 arg2))))
        ))

(defun lisp-form (vm fun)
  ())

(defun is-user-defined? (vm fun)
  (gethash fun (get vm 'etiquettes_resolues)))

(defun user-defined (vm fun)
  ;(set-case vm 1 (get-SP vm)) ;le FP recoit le SP
  (vmpush vm (- (get-SP vm) 1)) ;on empile SP - 1
  ;(vmpush vm (get-FP vm)) ;on empile FP
  (vmpush vm (get-PC vm)) ; on empile le PC
  (set-case vm 3 (+ 1 (gethash fun (get vm 'etiquettes_resolues))))) ;le PC recoit l'adresse de la fonction
  
