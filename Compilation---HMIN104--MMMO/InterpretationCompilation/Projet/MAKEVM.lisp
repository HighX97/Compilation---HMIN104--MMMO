;TESTS PAS TESTéS

;(COMP '(defun factorielle (n) (if (<= n 0) 1 (* n (factorielle (- n 1))))))
;(COMP '(defun fibonacci (n) (if (<= n 2) 1 (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;(VMEVAL 'vm '(defun factorielle (n) (if (<= n 0) 1 (* n (factorielle (- n 1))))))
;(VMEVAL 'vm '(defun fibonacci (n) (if (<= n 2) 1 (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
;(VMEVAL 'vm '(defun test_if (n) (if (<= n 0) (+ n n) (* n n))))

(defun MAKEVM (vm taille) 
	(set-pile 
		vm 
		taille
	)
)

(defun SETPILE (vm taille) 
	(setf 
		(get 
			vm 
			'pile
		) 
		(make-array 
			taille 
			:initial-element 
			0
		)
	)
	(SETCASE ;BP
		vm 
		0 
		8
	) 
	(SETCASE ;FP
		vm 
		1 
		8
	) 
	(SETCASE ;SP
		vm 
		2 
		8
	) 
	(SETCASE ;PC
		vm 
		3 
		(- 
			taille 
			1
		)
	) 
	(SETCASE ;CC (code counter)
		vm 
		4 
		(- 
			taille 
			1
		)
	) 
	(SETCASE ;FLT
		vm 
		5 
		0
	) 
	(SETCASE ;FEQ
		vm 
		6 
		0
	) 
	(SETCASE ;FGT
		vm 
		7 
		0
	) 

	(setf 
		(get 
			vm 
			'etiquettes_resolues
		) 
		(make-hash-table)
	)
	(setf 
		(get 
			vm 
			'references_avant
		) 
		(make-hash-table)
	)
	vm
)

(defun RESETVM (vm) 
	(MAKEVM 
		vm
	)
)

(defun RESETPILE (vm)
	(SETCASE 
		vm 
		1 
		8
	)
	(SETCASE 
		vm 
		2 
		8
	)
)

(defun VMEVAL (vm code)
	(if 
		(eq 
			(first 
				code
			) 
			'defun
		) 
		(progn 
			(CHARGEVM 
				vm 
				(COMP 
					code
				)
			) 
			(SETCASE 
				vm 
				3 
				(GETCC 
					vm
				)
			) 
			(second 
				code
			)
		)
		(progn 
			(CHARGEVM 
				vm 
				(COMP 
					code
				)
			)
			(READCODE 
				vm
			)
			(GETSPVALUE 
				vm
			)
		)
	)
)

(defun CHARGEVM (vm code) 
	(let 
		(
			(ghtrr 
				(get 
					vm 
					'etiquettes_resolues
				)
			) 
			(ghtra 
				(get 
					vm 
					'references_avant
				)
			) 
			(lhtrr 
				(make-hash-table)
			) 
			(lhtra 
				(make-hash-table)
			)
		) 
		(CHARGEVMACC 
			vm 
			code 
			ghtrr 
			ghtra 
			lhtrr 
			lhtra
		)
	)
)

(defun CHARGEVMACC (vm code ghtrr ghtra lhtrr lhtra) 
	(if 
		(atom 
			code
		)
		NIL
		(let 
			(instr 
				(first 
					code
				)
			)
			(cond
				(
					(eq 
						(first 
							instr
						) 
						:label
					)
					(let 
						(
							(addr 
								(GETCC 
									vm
								)
							) 
							(label 
								(second 
									instr
								)
							)
						)
						(if 
							(symbolp 
								(second 
									instr
								)
							) 
							(progn
								(LOADLABEL 
									addr 
									label 
									ghtrr 
									ghtra
								)
								(CHARGEVMACC 
									vm 
									(cdr 
										code
									) 
									ghtrr 
									ghtra 
									lhtrr 
									lhtra
								)
							)
							(progn
								(LOADLABEL 
									addr 
									label 
									lhtrr 
									lhtra
								)
								(CHARGEVMACC 
									vm 
									(cdr 
										code
									) 
									ghtrr 
									ghtra 
									lhtrr 
									lhtra
								)
							)
						)
					)
				)
				(T
					(SETCASE 
						vm 
						(GETCC 
							vm
						) 
						instr
					)
					(VMDECR 
						vm 
						4
					)
					(CHARGEVMACC 
						vm 
						(cdr 
							code
						) 
						ghtrr 
						ghtra 
						lhtrr 
						lhtra
					)
			    )
			)
		)
	)
)		   		   

(defun LOADVM (vm code) 
	(let 
		(
			(ghtrr 
				(get 
					vm 
					'etiquettes_resolues
				)
			)
			(ghtra 
				(get 
					vm 
					'references_avant
				)
			)
			(lhtrr 
				(make-hash-table)
			)
			(lhtra 
				(make-hash-table)
			)
		)
		(loop 
			for 
			instr 
			in 
			code 
			do 
			(cond
				(
					(eq 
						(first 
							instr
						) 
						:label
					)
					(let 
						(
							(addr 
								(GETCC 
									vm
								)
							) 
							(label 
								(second 
									instr
								)
							)
						)
						(if 
							(symbolp 
								(second 
									instr
								)
							)
							(LOADLABEL 
								addr 
								label 
								ghtrr 
								ghtra
							)
							(LOADLABEL 
								addr 
								label 
								lhtrr 
								lhtra
							)
						)
					)
				)
				(T
					(SETCASE 
						vm 
						(GETCC 
							vm
						) 
						instr
					)
					(VMDECR 
						vm 
						4
					)
				)
			)
		)
	)
) 

(defun LOADLABEL (addr label htrr htra)
	(let 
		(etat 
			T
		)
		(if 
			(gethash 
				label 
				htrr
			)
			(progn
				(setf 
					etat 
					NIL
				)
				(warn 
					"Etiquette déjà déclarée"
				)
			)
			(progn
				(setf 
					(gethash 
						label 
						htrr
					) 
					addr
				)
				(if (gethash 
						label 
						htra
					)
					NIL
					NIL
				)
			)
		)
		etat
	)
)

(defun LOADJMP (vm addr label htrr htra) 
	NIL
)

(defun LOADCODE (vm code) 
	(let 
		(
			(index 
				(GETCC 
					vm
				)
			)
		)
		(if
			(atom 
				code
			)
			NIL
			(progn 
				(SETCASE 
					vm 
					index 
					(car 
						code
					)
				)
				(VMDECR 
					vm 
					4
				)
				(LOADCODE 
					vm 
					(cdr 
						code
					)
				)
			)
        )
	)
	(values)
)
  
(defun READCODE (vm)
	(loop 
		while 
		(not 
			(eq 
				(GETCASE 
					vm 
					(GETPC 
						vm
					)
				) 
				0
			)
		)
		do
		(let 
			(
				(instr 
					(GETCASE 
						vm 
						(GETPC 
							vm
						)
					)
				)
			)
			(if 
				(atom 
					instr
				)
				(return)
				(cond
					(
						(eq 
							(first 
								instr
							) 
							:stack
						)
						(VMSTACK 
							vm 
							(second 
								instr
							)
						)
						(VMDECR 
							vm 3
						)
					)
					(
						(eq 
							(first 
								instr
							) 
							:call
						)
						(VMCALL 
							vm 
							(second 
								instr
							)
						)
						(VMDECR 
							vm 
							3
						)
					)
				    (
						(eq 
							(first 
								instr
							) 
							:const
						)
						(VMCONST 
							vm 
							(second 
								instr
							)
						)
						(VMDECR 
							vm 
							3
						)
					)
				    (
						(eq 
							(first 
								instr
							) 
							:var
						)
						(VMVAR 
							vm 
							(second 
								instr
							)
						)
						(VMDECR 
							vm 
							3
						)
					)
				    (
						(eq 
							(first 
								instr
							) 
							:set-var
						)
						(VMSETVAR 
							vm
						)
						(VMDECR 
							vm 
							3
						)
					)
				    (
						(eq 
							(first 
								instr
							) 
							:rtn
						)
						(VMRTN 
							vm
						)
					)
				    (
						(eq 
							(first 
								instr
							) 
							:skip
						)
						(VMSKIP 
							vm 
							(second 
								instr
							)
						)
						(VMDECR 
							vm 
							3
						)
					)
				    (
						(eq 
							(first 
								instr
							) 
							:skipnil
						)
						(VMSKIPNIL 
							vm 
							(second 
								instr
							)
						)
						(VMDECR 
							vm 
							3
						)
					)
				    (
						(eq 
							(first 
								instr
							) 
							:skiptrue
						)
						(VMSKIPTRUE 
							vm 
							(second 
								instr
							)
						)
						(VMDECR 
							vm 
							3
						)
					)
				    (
						(eq 
							(first 
								instr
							) 
							:jump
						)
						(VMJUMP 
							vm 
							(second 
								instr
							)
						)
						(VMDECR 
							vm 
							3
						)
					)
				    (
						(eq 
							(first 
								instr
							) 
							:load
						)
						(VMLOAD 
							vm 
							(second 
								instr)
						)
						(VMDECR 
							vm 
							3
						)
					)
				    (
						(eq 
							(first 
								instr
							) 
							:store
						)
						(VMSTORE 
							vm 
							(second 
								instr
							)
						)
						(VMDECR 
							vm 
							3
						)
					)
				)
			)
		)
	)
	(GETSPVALUE 
		vm
	)
)

(defun SHOWVM (vm) 
	(print 
		'COUNTERS
	) 
	(print 
		(list 
			:BP 
			(GETCASE 
				vm 
				0
			)
		)
	) 
	(print 
		(list 
			:FP 
			(GETCASE 
				vm 
				1
			)
		)
	) 
	(print 
		(list 
			:SP 
			(GETCASE 
				vm 
				2
			)
		)
	) 
	(print 
		(list 
			:PC 
			(GETCASE 
				vm 
				3
			)
		)
	) 
	(print 
		(list 
			:CC 
			(GETCASE 
				vm 
				4
			)
		)
	) 
	(print 
		'FLAGS
	) 
	(print 
		(list 
			:FLT 
			(GETCASE 
				vm 
				5
			)
		)
	) 
	(print 
		(list 
			:FEQ 
			(GETCASE 
				vm 
				6
			)
		)
	) 
	(print 
		(list 
			:FGT 
			(GETCASE 
				vm 
				7
			)
		)
	)		
	(print 
		'PILE
	) 
	(print 
		(GETPILE 
			vm
		)
	) 
	(values)
)	

;Renvoie le tableau contenant la pile
(defun GETPILE (vm)	
	(get 
		vm 
		'pile
	)
)	

;Renvoie la valeur située à la position index
(defun GETCASE (vm index) 
	(aref 
		(GETPILE 
			vm
		) 
		index
	)
)

;Remplace la valeur située à la position index par val
(defun SETCASE (vm index val) 
	(setf 
		(aref 
			(GETPILE 
				vm
			) 
			index
		) 
		val
	)
)

;Revoie le BP
(defun GETBP (vm)
	(aref 
		(get 
			vm 
			'pile
		) 
		0
	)
)

;Renvoie le FP
(defun GETFP (vm) 
	(aref 
		(get 
			vm 
			'pile
		) 
		1
	)
)

;Renvoie le SP
(defun GETSP (vm) 
	(aref 
		(get 
			vm 
			'pile
		) 
		2
	)
)

;Revoie le PC
(defun GETPC (vm) 
	(aref 
		(get 
			vm 
			'pile
		) 
		3
	)
)

;Renvoie le CC
(defun GETCC (vm) 
	(aref 
		(get 
			vm 
			'pile
		) 
		4
	)
)

;Modifie la valeur du SP
(defun SETSP (vm val) 
	(setf 
		(aref 
			(get 
				vm 
				'pile
			) 
			2
		) 
		val
	)
)

;Modifie la valeur du FP
(defun SETFP (vm val) 
	(setf 
		(aref 
			(get 
				vm 
				'pile
			) 
			1
		) 
		val
	)
)

;Renvoie la valeur du SP +/- index
(defun SP (vm op index) 
	(cond 
		(
			(eq 
				'- 
				op
			)
			(- 
				(GETSP 
					vm
				) 
				index
			)
		)
		(
			(eq 
				'+ 
				op
			) 	
			(+ 
				(GETSP 
					vm
				) 
				index
			)
		)
	)
)

;Renvoie la valeur du FP +/- index
(defun FP (vm op index) 
	(cond 
		(
			(eq 
				'- 
				op
			)
			(- 
				(GETFP 
					vm
				) 
				index
			)
		)
		(
			(eq 
				'+ 
				op
			) 	
			(+ 
				(GETFP 
					vm
				) 
				index
			)
		)
	)
)

(defun GETSPVALUE (vm) 
	(aref 
		(get 
			vm 
			'pile
		) 
		(- 
			(GETSP 
				vm
			) 
			1
		)
	)
)

(defun GETFPVALUE (vm) 
	(GETCASE 
		vm 
		(GETFP 
			vm
		)
	)
)
  

;Push
(defun VMPUSH (vm val) 
	(SETCASE 
		vm 
		(GETSP 
			vm
		) 
		val
	)
	(VMINCR 
		vm 
		2
	)	
)	

;Pop
(defun VMPOP (vm) 
	(progn 
		(VMDECR 
			vm 
			2
		)
		(GETCASE 
			vm 
			(GETSP 
				vm
			)
		)
	)
)	

(defun VMCONST (vm lit) 
	(VMPUSH 
		vm 
		lit
	)	
)	

(defun VMVAR (vm n) 
	(VMPUSH 
		vm 
		(GETCASE 
			vm 
			(FP 
				vm 
				'- 
				(+ 
					1 
					(- 
						(GETFPVALUE 
							vm
						) 
						n
					)
				)
			)
		)
	)
)

(defun VMSETVAR (vm) 
	NIL
)

(defun VMSTACK (vm n) 
	(VMPUSH 
		vm 
		(+ 
			2 
			n
		)
	)
	(SETCASE 
		vm 
		1 
		(- 
			(GETSP 
				vm
			) 
			1
		)
	)
)

(defun VMCALL (vm f) 
	(cond
		(
			(ISOPERATOR? 
				f
			) 
			(OPERATOR 
				vm 
				f
			)
		)
		(
			(ISLISPFORM? 
				vm 
				f
			) 
			(LISPFORM 
				vm 
				f
			)
		)
		(
			(ISUSERDEFINED? 
				vm 
				f
			) 
			(USERDEFINED 
				vm 
				f
			)
		)
	)
)		

(defun VMRTN (vm) 
	(let 
		(
			(val 
				(VMPOP 
					vm
				)
			) 
			(nbpar 
				(VMPOP 
					vm
				)
			) 
			(adret 
				(VMPOP 
					vm
				)
			) 
			(osp 
				(VMPOP 
					vm
				)
			)
		) 
		(SETFP 
			vm 
			(- 
				osp 
				1
			)
		)
		(SETSP 
			vm 
			(- 
				osp 
				(- 
					nbpar 
					3
				)
			)
		)
		(VMPUSH 
			vm 
			val
		)
		(SETCASE 
			vm 
			3 
			(- 
				adret 
				1
			)
		)
	)
)

(defun VMSKIP (vm n) 
	(SETCASE 
		vm 
		3 
		(- 
			(GETPC 
				vm
			) 
			n
		)
	)
)

(defun VMSKIPNIL (vm n) 
	(let 
		(val 
			(VMPOP 
				vm
			)
		)
		(if 
			(eq 
				nil 
				val
			)
			(VMSKIP 
				vm 
				n
			)
			NIL
		)
	)
)

(defun VMSKIPTRUE (vm n) 
	(let 
		(val 
			(VMPOP 
				vm
			)
		)
		(if 
			(eq 
				T 
				val
			)
			(VMSKIP 
				vm 
				n
			)
			NIL
		)
	)
)

(defun VMJUMP (vm n) 
	(SETCASE 
		vm 
		3 
		(+ 
			1 
			(gethash 
				fun 
				(get 
					vm 
					'etiquettes_resolues
				)
			)
		)
	)
)

(defun VMLOAD (vm n) 
	NIL
)	 

(defun VMSTORE (vm n) 
	(let 
		(
			(val 
				(VMPOP 
					vm
				)
			) 
			(nbparam 
				(VMPOP 
					vm
				)
			) 
			(adret 
				(VMPOP 
					vm
				)
			) 
			(osp 
				(VMPOP 
					vm
				)
			)
		)
		(VMINCR 
			vm 
			1
		)
		(VMPUSH 
			vm 
			val
		)
		(VMPUSH 
			vm 
			(+ 
				1 
				osp
			)
		)
		(VMPUSH 
			vm 
			adret
		)
		(VMPUSH 
			vm 
			(+ 
				1 
				nbparam
			)
		)
	)
)	

(defun ISOPERATOR? (fun) 
	(or 
		(eq 
			'+ 
			fun
		)
		(eq 
			'- 
			fun
		)
		(eq 
			'* 
			fun
		)
		(eq 
			'/ 
			fun
		)
		(eq 
			'++ 
			fun
		)
		(eq 
			'-- 
			fun
		)
		(eq 
			'> 
			fun
		)
		(eq 
			'< 
			fun
		)
		(eq 
			'= 
			fun
		)
		(eq 
			'<= 
			fun
		)
		(eq 
			'>= 
			fun
		)
	)
)

(defun OPERATOR (vm fun) 
	(let 
		(
			(src 
				(SP 
					vm 
					'- 
					1
				)
			) 
			(dest 
				(SP 
					vm 
					'- 
					2
				)
			)
		)
		(cond 
			(
				(eq 
					'+ 
					fun
				)
				(VMADD 
					vm 
					src 
					dest
				)
			)
			(
				(eq 
					'- 
					fun
				)	
				(VMSUB 
					vm 
					src 
					dest
				)
			)
			(
				(eq 
					'* 
					fun
				)
				(VMMUL 
					vm 
					src 
					dest
				)
			)
			(
				(eq 
					'/ 
					fun
				)
				(VMDIV 
					vm 
					src 
					dest
				)
			)
			(
				(eq 
					'++ 
					fun
				)
				(VMINCR 
					vm 
					src
				)
			)
			(
				(eq 
					'-- 
					fun
				)
				(VMDECR 
					vm 
					src
				)
			)
			(
				(eq 
					'= 
					fun
				)
				(VMCMP 
					vm 
					'= 
					src 
					dest
				)
			)
			(
				(eq 
					'> 
					fun
				)
				(VMCMP 
					vm 
					'> 
					src 
					dest
				)
			)
			(
				(eq 
					'< 
					fun
				)
				(VMCMP 
					vm 
					'< 
					src 
					dest
				)
			)
			(
				(eq 
					'<= 
					fun
				)
				(VMCMP 
					vm 
					'<= 
					src 
					dest
				)
			)
			(
				(eq 
					'>= 
					fun
				)
				(VMCMP 
					vm 
					'>= 
					src 
					dest
				)
			)
		)
	)
)

(defun VMADD (vm src dest) 
	(let 
		(
			(v2 
				(VMPOP 
					vm
				)
			) 
			(v1 
				(VMPOP 
					vm
				)
			)
		)
		(VMPUSH 
			vm 
			(+ 
				v1 
				v2
			)
		)
	)
)

(defun VMSUB (vm src dest) 
	(let 
		(
			(v2 
				(VMPOP 
					vm
				)
			) 
			(v1 
				(VMPOP 
					vm
				)
			)
		)
		(VMPUSH 
			vm 
			(- 
				v1 
				v2
			)
		)
	)
)

(defun VMMUL (vm src dest) 
	(let 
		(
			(v2 
				(VMPOP 
					vm
				)
			) 
			(v1 
				(VMPOP 
					vm
				)
			)
		)
		(VMPUSH 
			vm 
			(* 
				v1 
				v2
			)
		)
	)
)

(defun VMDIV (vm src dest) 
	(let 
		(
			(v2 
				(VMPOP 
					vm
				)
			) 
			(v1 
				(VMPOP 
					vm
				)
			)
		)
		(VMPUSH 
			vm 
			(/ 
				v1 
				v2
			)
		)
	)
)

(defun VMINCR (vm dest) 
	(let 
		(
			(v1 
				(GETCASE 
					vm 
					dest
				)
			)
		)
		(SETCASE 
			vm 
			dest 
			(+ 
				v1 
				1
			)
		)
	)
)

(defun VMDECR (vm dest) 
	(let 
		(
			(v1 
				(GETCASE 
					vm 
					dest
				)
			)
		)
		(SETCASE 
			vm 
			dest 
			(- 
				v1 
				1
			)
		)
	)
)

(defun VMCMP (vm op src dest) 
	(let 
		(
			(v2 
				(VMPOP 
					vm
				)
			) 
			(v1 
				(VMPOP 
					vm
				)
			)
		)
		(case op
			(= 
				(VMPUSH 
					vm 
					(= 
						v1 
						v2
					)
				)
			)
			(> 
				(VMPUSH 
					vm 
					(> 
						v1 
						v2
					)
				)
			)
			(< 
				(VMPUSH 
					vm 
					(< 
						v1 
						v2
					)
				)
			)
			(<= 
				(VMPUSH 
					vm 
					(<= 
						v1 
						v2
					)
				)
			) 
			(>= 
				(VMPUSH 
					vm 
					(>= 
						v1 
						v2
					)
				)
			)
		)
    )
)

(defun RESETFLAGS (vm) 
	(SETCASE ;FLT
		vm 
		3 
		0
	) 
	(SETCASE ;FEQ
		vm 
		4 
		0
	) 
	(SETCASE ;FGT
		vm 
		5 
		0
	) 
)

(defun ISLISPFORM? (vm fun)
	(case 
		fun 
		('print 
			(print 
				(VMPOP 
					vm
				)
			)
		)
		('get 
			(let 
				(
					(arg2 
						(VMPOP 
							vm
						)
					) 
					(arg1 
						(VMPOP 
							vm
						)
					)
				)
				(VMPUSH 
					vm 
					(get 
						arg1 
						arg2
					)
				)
			)
		)
		('make-hash-table 
			(VMPUSH 
				vm 
				(make-hash-table)
			)
		) 
		('atom 
			(VMPUSH 
				vm 
				(atom 
					(VMPOP 
						vm
					)
				)
			)
		)
		('first 
			(VMPUSH 
				vm 
				(first 
					(VMPOP 
						vm
					)
				)
			)
		)
		('second
			(VMPUSH 
				vm 
				(second 
					(VMPOP 
						vm
					)
				)
			)
		)
		('cdr
			(VMPUSH 
				vm 
				(cdr 
					(VMPOP 
						vm
					)
				)
			)
		)
		('eq
			(VMPUSH 
				vm 
				(eq 
					(VMPOP 
						vm
					) 
					(VMPOP 
						vm
					)
				)
			)
		)
		('symbolp
			(VMPUSH 
				vm 
				(symbolp 
					(VMPOP 
						vm
					)
				)
			)
		)
		('aref
			(let 
				(
					(arg2 
						(VMPOP 
							vm
						)
					) 
					(arg1 
						(VMPOP 
							vm
						)
					)
				)
			(VMPUSH 
				vm 
					(aref 
						arg1 
						arg2
					)
				)
			)
		)
		('GETCC
			(VMPUSH 
				vm 
				(GETCC 
					(VMPOP 
						vm
					)
				)
			)
		)
		('SETCASE
			(let 
				(
					(arg3 
						(VMPOP 
							vm
						)
					) 
					(arg2 
						(VMPOP 
							vm
						)
					) 
					(arg1 
						(VMPOP 
							vm
						)
					)
				)
				(VMPUSH 
					vm 
					(SETCASE 
						arg1 
						arg2 
						arg3
					)
				)
			)
		)
		('GETPILE
			(VMPUSH 
				vm 
				(GETPILE 
					(VMPOP 
						vm
					)
				)
			)
		)
		('VMDECR
			(let 
				(
					(arg2 
						(VMPOP 
							vm
						)
					) 
					(arg1 
						(VMPOP 
							vm
						)
					)
				)
				(VMPUSH 
					vm 
					(VMDECR 
						arg1 
						arg2
					)
				)
			)
		)
		('GETCASE
			(let 
				(
					(arg2 
						(VMPOP 
							vm
						)
					) 
					(arg1 
						(VMPOP 
							vm
						)
					)
				)
				(VMPUSH 
					vm 
					(GETCASE 
						arg1 
						arg2
					)
				)
			)
		)
	)
)

(defun LISPFORM (vm fun) 
	NIL
)

(defun ISUSERDEFINED? (vm fun) 
	(gethash 
		fun 
		(get 
			vm 
			'etiquettes_resolues
		)
	)
)

(defun USERDEFINED (vm fun) 
	(VMPUSH ;On empile SP - 1
		vm 
		(- 
			(GETSP 
				vm
			) 
			1
		)
	) 
	(VMPUSH ;On empile le PC
		vm 
		(GETPC 
			vm
		)
	) 
	(SETCASE ;Le PC reçoit l'adresse de la fonction
		vm 
		3 
		(+ 
			1 
			(gethash 
				fun 
				(get 
					vm 
					'etiquettes_resolues
				)
			)
		)
	)
) 
  
;SOUS FORME LINEAIRE

(defun MAKEVM (vm taille) (set-pile vm taille))
(defun SETPILE (vm taille) (setf (get vm 'pile) (make-array taille :initial-element 0)) (SETCASE vm 0 8) (SETCASE vm 1 8) (SETCASE vm 2 8) (SETCASE vm 3 (- taille 1)) (SETCASE vm 4 (- taille 1)) (SETCASE vm 5 0) (SETCASE vm 6 0) (SETCASE vm 7 0) (setf (get vm 'etiquettes_resolues) (make-hash-table)) (setf (get vm 'references_avant) (make-hash-table)) vm)
(defun RESETVM (vm) (MAKEVM vm))
(defun RESETPILE (vm) (SETCASE vm 1 8) (SETCASE vm 2 8))
(defun VMEVAL (vm code) (if (eq (first code) 'defun) (progn (CHARGEVM vm (COMP code)) (SETCASE vm 3 (GETCC vm)) (second code)) (progn (CHARGEVM vm (COMP code)) (READCODE vm) (GETSPVALUE vm))))
(defun CHARGEVM (vm code) (let ((ghtrr (get vm 'etiquettes_resolues)) (ghtra (get vm 'references_avant)) (lhtrr (make-hash-table)) (lhtra (make-hash-table))) (CHARGEVMACC vm code ghtrr ghtra lhtrr lhtra)))
(defun CHARGEVMACC (vm code ghtrr ghtra lhtrr lhtra) (if (atom code) NIL (let (instr (first code)) (cond ((eq (first instr) :label) (let ((addr (GETCC vm)) (label (second instr))) (if (symbolp (second instr)) (progn (LOADLABEL addr label ghtrr ghtra) (CHARGEVMACC vm (cdr code) ghtrr ghtra lhtrr lhtra)) (progn (LOADLABEL addr label lhtrr lhtra) (CHARGEVMACC vm (cdr code) ghtrr ghtra lhtrr lhtra))))) (T (SETCASE vm (GETCC vm) instr) (VMDECR vm 4) (CHARGEVMACC vm (cdr code) ghtrr ghtra lhtrr lhtra))))))		   
(defun LOADVM (vm code) (let ((ghtrr (get vm 'etiquettes_resolues)) (ghtra (get vm 'references_avant)) (lhtrr (make-hash-table)) (lhtra (make-hash-table))) (loop for instr in code do (cond ((eq (first instr) :label) (let ((addr (GETCC vm)) (label (second instr))) (if (symbolp (second instr)) (LOADLABEL addr label ghtrr ghtra) (LOADLABEL addr label lhtrr lhtra)))) (T (SETCASE vm (GETCC vm) instr) (VMDECR vm 4))))))
(defun LOADLABEL (addr label htrr htra) (let (etat T) (if (gethash label htrr) (progn (setf etat NIL) (warn "Etiquette déjà déclarée")) (progn (setf (gethash label htrr) addr) (if (gethash label htra) NIL NIL))) etat))
(defun LOADJMP (vm addr label htrr htra) NIL)
(defun LOADCODE (vm code) (let ((index (GETCC vm))) (if (atom code) NIL (progn (SETCASE vm index (car code)) (VMDECR vm 4) (LOADCODE vm (cdr code))))) (values))
(defun READCODE (vm) (loop while (not (eq (GETCASE vm (GETPC vm)) 0)) do (let ((instr (GETCASE vm (GETPC vm)))) (if (atom instr) (return) (cond ((eq (first instr) :stack) (VMSTACK vm (second instr)) (VMDECR vm 3)) ((eq (first instr) :call) (VMCALL vm (second instr)) (VMDECR vm 3)) ((eq (first instr) :const) (VMCONST vm (second instr)) (VMDECR vm 3)) ((eq (first instr) :var) (VMVAR vm (second instr)) (VMDECR vm 3)) ((eq (first instr) :set-var) (VMSETVAR vm) (VMDECR vm 3)) ((eq (first instr) :rtn) (VMRTN vm)) ((eq (first instr) :skip) (VMSKIP vm (second instr)) (VMDECR vm 3)) ((eq (first instr) :skipnil) (VMSKIPNIL vm (second instr)) (VMDECR vm 3)) ((eq (first instr) :skiptrue) (VMSKIPTRUE vm (second instr)) (VMDECR vm 3)) ((eq (first instr) :jump) (VMJUMP vm (second instr)) (VMDECR vm 3)) ((eq (first instr) :load) (VMLOAD vm (second instr)) (VMDECR vm 3)) ((eq (first instr) :store) (VMSTORE vm (second instr)) (VMDECR vm 3)))))) (GETSPVALUE vm))
(defun SHOWVM (vm) (print 'COUNTERS) (print (list :BP (GETCASE vm 0))) (print (list :FP (GETCASE vm 1))) (print (list :SP (GETCASE vm 2))) (print (list :PC (GETCASE vm 3))) (print (list :CC (GETCASE vm 4))) (print 'FLAGS) (print (list :FLT (GETCASE vm 5))) (print (list :FEQ (GETCASE vm 6))) (print (list :FGT (GETCASE vm 7))) (print 'PILE) (print (GETPILE vm)) (values))
(defun GETPILE (vm)	(get vm 'pile))
(defun GETCASE (vm index) (aref (GETPILE vm) index))
(defun SETCASE (vm index val) (setf (aref (GETPILE vm) index) val))
(defun GETBP (vm) (aref (get vm 'pile) 0))
(defun GETFP (vm) (aref (get vm 'pile) 1))
(defun GETSP (vm) (aref (get vm 'pile) 2))
(defun GETPC (vm) (aref (get vm 'pile) 3))
(defun GETCC (vm) (aref (get vm 'pile) 4)) 
(defun SETSP (vm val) (setf (aref (get vm 'pile) 2) val))
(defun SETFP (vm val) (setf (aref (get vm 'pile) 1) val))	 
(defun SP (vm op index) (cond ((eq '- op) (- (GETSP vm) index)) ((eq '+ op) (+ (GETSP vm) index))))
(defun FP (vm op index) (cond ((eq '- op) (- (GETFP vm) index)) ((eq '+ op) (+ (GETFP vm) index)))) 
(defun GETSPVALUE (vm) (aref (get vm 'pile) (- (GETSP vm) 1)))
(defun GETFPVALUE (vm) (GETCASE vm (GETFP vm))) 
(defun VMPUSH (vm val) (SETCASE vm (GETSP vm) val) (VMINCR vm 2))
(defun VMPOP (vm) (progn (VMDECR vm 2) (GETCASE vm (GETSP vm))))
(defun VMCONST (vm lit) (VMPUSH vm lit)	)	
(defun VMVAR (vm n) (VMPUSH vm (GETCASE vm (FP vm '- (+ 1 (- (GETFPVALUE vm) n))))))
(defun VMSETVAR (vm) NIL)	
(defun VMSTACK (vm n) (VMPUSH vm (+ 2 n)) (SETCASE vm 1 (- (GETSP vm) 1)))	
(defun VMCALL (vm f) (cond ((ISOPERATOR? f) (OPERATOR vm f)) ((ISLISPFORM? vm f) (LISPFORM vm f)) ((ISUSERDEFINED? vm f) (USERDEFINED vm f))))		
(defun VMRTN (vm) (let ((val (VMPOP vm)) (nbpar (VMPOP vm)) (adret (VMPOP vm)) (osp (VMPOP vm))) (SETFP vm (- osp 1)) (SETSP vm (- osp (- nbpar 3))) (VMPUSH vm val) (SETCASE vm 3 (- adret 1))))
(defun VMSKIP (vm n) (SETCASE vm 3 (- (GETPC vm) n)))  
(defun VMSKIPNIL (vm n) (let (val (VMPOP vm)) (if (eq nil val) (VMSKIP vm n) NIL)))
(defun VMSKIPTRUE (vm n) (let (val (VMPOP vm)) (if (eq T val) (VMSKIP vm n) NIL))) 
(defun VMJUMP (vm n) (SETCASE vm 3 (+ 1 (gethash fun (get vm 'etiquettes_resolues)))))
(defun VMLOAD (vm n) NIL)		
(defun VMSTORE (vm n) (let ((val (VMPOP vm)) (nbparam (VMPOP vm)) (adret (VMPOP vm)) (osp (VMPOP vm))) (VMINCR vm 1) (VMPUSH vm val) (VMPUSH vm (+ 1 osp)) (VMPUSH vm adret) (VMPUSH vm (+ 1 nbparam))))	
(defun ISOPERATOR? (fun) (or (eq '+ fun) (eq '- fun) (eq '* fun) (eq '/ fun) (eq '++ fun) (eq '-- fun) (eq '> fun) (eq '< fun) (eq '= fun) (eq '<= fun) (eq '>= fun)))	
(defun OPERATOR (vm fun) (let ((src (SP vm '- 1)) (dest (SP vm '- 2))) (cond ((eq '+ fun) (VMADD vm src dest)) ((eq '- fun) (VMSUB vm src dest)) ((eq '* fun) (VMMUL vm src dest)) ((eq '/ fun) (VMDIV vm src dest)) ((eq '++ fun) (VMINCR vm src)) ((eq '-- fun) (VMDECR vm src)) ((eq '= fun) (VMCMP vm '= src dest)) ((eq '> fun) (VMCMP vm '> src dest)) ((eq '< fun) (VMCMP vm '< src dest)) ((eq '<= fun) (VMCMP vm '<= src dest)) ((eq '>= fun) (VMCMP vm '>= src dest)))))
(defun VMADD (vm src dest) (let ((v2 (VMPOP vm)) (v1 (VMPOP vm))) (VMPUSH vm (+ v1 v2))))	
(defun VMSUB (vm src dest) (let ((v2 (VMPOP vm)) (v1 (VMPOP vm))) (VMPUSH vm (- v1 v2))))	
(defun VMMUL (vm src dest) (let ((v2 (VMPOP vm)) (v1 (VMPOP vm))) (VMPUSH vm (* v1 v2))))	
(defun VMDIV (vm src dest) (let ((v2 (VMPOP vm)) (v1 (VMPOP vm))) (VMPUSH vm (/ v1 v2))))	
(defun VMINCR (vm dest) (let ((v1 (GETCASE vm dest))) (SETCASE vm dest (+ v1 1))))	
(defun VMDECR (vm dest) (let ((v1 (GETCASE vm dest))) (SETCASE vm dest (- v1 1))))
(defun VMCMP (vm op src dest) (let ((v2 (VMPOP vm)) (v1 (VMPOP vm))) (case op (= (VMPUSH vm (= v1 v2))) (> (VMPUSH vm (> v1 v2))) (< (VMPUSH vm (< v1 v2))) (<= (VMPUSH vm (<= v1 v2))) (>= (VMPUSH vm (>= v1 v2))))))
(defun RESETFLAGS (vm) (SETCASE vm 3 0) (SETCASE vm 4 0) (SETCASE vm 5 0))
(defun ISLISPFORM? (vm fun) (case fun ('print (print (VMPOP vm))) ('get (let ((arg2 (VMPOP vm)) (arg1 (VMPOP vm))) (VMPUSH vm (get arg1 arg2)))) ('make-hash-table (VMPUSH vm (make-hash-table))) ('atom (VMPUSH vm (atom (VMPOP vm)))) ('first (VMPUSH vm (first (VMPOP vm)))) ('second (VMPUSH vm (second (VMPOP vm)))) ('cdr (VMPUSH vm (cdr (VMPOP vm)))) ('eq (VMPUSH vm (eq (VMPOP vm) (VMPOP vm)))) ('symbolp (VMPUSH vm (symbolp (VMPOP vm)))) ('aref (let ((arg2 (VMPOP vm)) (arg1 (VMPOP vm))) (VMPUSH vm (aref arg1 arg2)))) ('GETCC (VMPUSH vm (GETCC (VMPOP vm)))) ('SETCASE (let ((arg3 (VMPOP vm)) (arg2 (VMPOP vm)) (arg1 (VMPOP vm))) (VMPUSH vm (SETCASE arg1 arg2 arg3)))) ('GETPILE (VMPUSH vm (GETPILE (VMPOP vm)))) ('VMDECR (let ((arg2 (VMPOP vm)) (arg1 (VMPOP vm))) (VMPUSH vm (VMDECR arg1 arg2)))) ('GETCASE (let ((arg2 (VMPOP vm)) (arg1 (VMPOP vm))) (VMPUSH vm (GETCASE arg1 arg2))))))
(defun LISPFORM (vm fun) NIL)
(defun ISUSERDEFINED? (vm fun) (gethash fun (get vm 'etiquettes_resolues)))
(defun USERDEFINED (vm fun) (VMPUSH vm (- (GETSP vm) 1)) (VMPUSH vm (GETPC vm)) (SETCASE vm 3 (+ 1 (gethash fun (get vm 'etiquettes_resolues))))) 