Machine (virtuelle)

Structure de données +API

|Mémoire
|Pile
|Registres
|nom

vm-apply vm vm load

(make-vm 'mavm 10000);10000 taille mémoire
initialiser les registres
crér la mémoire
Implémentations | propriété des symboles
				| get

API d'acès à la machine
	(get-register 'mavm 'R0) =~ get
	(set-register 'mavm 'R0 <val> -> (setf (get ____) ___)
	(get-memory 'mavm <addr>)

mémoire[code,tas,pile] - Pas de tas dans la VM - SegmentationFault, pile écrase TAS - code --> ___ <-- pile

2-Exécution

(defun vm-run (vm)
	;pas de récursion
	(loop while +
		do 
		(vm-run-instru vm (get-memory vm (get_register vm 'CO))))
	)

vm-run-instr
arrêt de la machine 	| échapement
						| interupteur dans la VM 	| get-halt
													| set-halt
(defun vm-run-instr (vm instr)
	(ecase (car inst)
		(mov __)	;incremente le compteur ordinal
		(push ___)	;incremente le compteur ordinal
		(pop ___)	;incremente le compteur ordinal
		(add ___)	;incremente le compteur ordinal
		...
		(jmp -> positionne le compteur ordinal)
		(halt -> set-halt)
		(rtn -> manipulation de pile + position du compteur ordinal)))		

vm-apply
(vm-apply vm 'fibo 30 ())
->	| (vm-load (l??vm (lisp2li '(fibo 30))))
->	| + halt
->	| + return le compteur de R0

3-Édition de liens, résolution de symbole
(adr foo) -> adress 2392
.
.
.
(jmp foo) -> (jmp 2392)

2 structure de données

symbole - > adresse

table de hachage
|make-hashtable
|get hash

-symboles resolus -> adresse du symbole
-reference en avant -> list d'adresses des références en avant

2 types d'étiquettes

symboles globaux
|=nom de fonctions = symbole = unique
|appartien structure de donnée VM peut etre irrésolu

symbol locaux = entiers pas unique != position

(defun vm-load (vm code)
	;pas de recursion
	;adresse de chargement
	;table de hachage symbole resolus TSR
	;table de hachage reférence en avant TRA
	(let ((adrr (get-load adr vm))
		(gtra (get-tra vm))
		(gtsr (get-tsr vm))
		(ltra (make-hastable))
		(ltsr (make-hastable)))
	(loop while code
		;instruction normales
		;->	|copie en mémoire
		;	|increment l'adresse de chargement
		;label
		;->	|on enregistre le symbole dans la TSR
		;jmp ~ saut
		;->	|enregistré dans TSR
		;	|->on rempalce
		;	|->on enregistre dans TRA
		;	|on fecctue un copie en mémoire
		;	|si le symbole est déja dans TSR, on déclanche un warning
		;(let ((adr)))
		do
		(let ((instr (car code)
			(set-f code (cdr code))))
			;charger l'instruction
			(set-f adr (+ 1 adr))	;on ne le fait pas si c'est un label, à chaque instruction qu'on charge on incremente 
									;l'adresse et stock là ou l'on arrive
			(cond 
				(eq (car instr) label)
					(if (intergep (second instr))
						(vm-load-label adr ltsr ltra instr)
						(vm-load-label adr gtsr gtra instr))
				(eq (car instr) jump)
					(if (intergep (second instr))
						(vm-load-jump adr ltsr ltra instr)
						(vm-load-jump adr gtsr gtra instr)))))))

vm-run -> 	On suppose ue lorsque l'on tmbe sur un symbole non résolu, c'est probablement une fonction prédéfinie. 
			On fait donc appel à apply.
			Il faut récupérer les arguments dans la pile.
			Il faut savoir combien en prendre.

(foo 1 2  3)
	(push 1)
	(push 2)
	(push 3)
	(mov 3 R4)
	(JSR foo) -> (apply 'foo (1 2 3)) ; (1 2 3) doit être construit en les récupérants dans la pile. Le nombre d'argument dans enregistrer dans un registre différent de R0 par exemple R4.