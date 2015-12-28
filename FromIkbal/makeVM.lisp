(defun make-vm (nom taille)
  (setf (get nom :memory) (make-array taille))
  (setf (get nom :debCode) (- taille (* (floor (/ taille 3)) 2)));depend de taille 
  (setf (get nom :adrCode) (get nom :debCode));adresse pour charger dans le code
  (setf (get nom :FPP) 0);drapeau plus petit
  (setf (get nom :FPG) 0);drapeau plus grand
  (setf (get nom :FEQ) 0);drapeau egal
  (setf (get nom :ON) 0);interrupteur de la VM  on / off
  (set-register nom 'R0 0)
  (set-register nom 'R1 0)
  (set-register nom 'R2 0)
  (set-register nom 'SP 0)
  (set-register nom 'BP 0)
  (set-register nom 'FP 0) 
  (set-register nom 'CO (- taille (* (floor (/ taille 3)) 2)))
  (setf (get nom 'TRA) (make-hash-table)) ;table pour les ref en avance
  (setf (get nom 'TSR) (make-hash-table)) 
) 

;permet d'acceder à la valeur du registre
(defun get-register (nomvm registre)
  (get nomvm registre))

;permet de changer la valeur du registre
(defun set-register (nomvm registre valeur)
  (setf (get nomvm registre) valeur))

;permet d'acceder à la valeur à l'adresse dans la memoire
(defun get-memory (nomvm adresse)
  (aref (get nomvm :memory) adresse))

;permet de changer la valeur à l'adresse dans la memoire
(defun set-memory (nomvm adresse valeur)
  (setf (aref (get nomvm :memory) adresse) valeur))

;met la valeur dans la pile
;pas sur de ce qu'il faut faire 
;si c'est bon il faut que je mette un if pour eviter que ca aille dans le code
(defun vm-push (nomvm valeur)
  (set-register nomvm 'SP (+ (get-register nomvm 'SP) 1))
  (set-memory nomvm (get-register nomvm 'SP) valeur)
)

;;si c'est bon il faut que je mette un if pour eviter que ca aille dans le code
;deplace le SP
(defun vm-pop (nomvm)
  (set-register nomvm 'SP (- (get-register nomvm 'SP) 1)) 
) 

