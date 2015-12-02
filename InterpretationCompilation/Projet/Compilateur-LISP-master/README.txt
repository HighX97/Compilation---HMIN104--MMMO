LISP Compiler - GMIN 108 semester project (UM2) - 2012

(load "run.lisp")
charge les fichiers lisp2li, evalli, litovm, makevm
la vm est creee par defaut avec le nom 'vm

expr est le code en lisp a traiter

lisp2li :
(lisp2li expr ())
evalli :
(meval expr)
litoasm :
(comp expr)
vmeval:
(vmeval 'vm expr)

(lisp2li '(defun fibo (n) (if (<= n 0) 1 (+ (fibo (- n 1)) (fibo (- n 2))))) ())

(meval '(defun fibo (n) (if (<= n 0) 1 (+ (fibo (- n 1)) (fibo (- n 2))))))
(meval '(fibo 10))

(comp '(defun fibo (n) (if (<= n 0) 1 (+ (fibo (- n 1)) (fibo (- n 2))))))

(vmeval 'vm '(defun fibo (n) (if (<= n 0) 1 (+ (fibo (- n 1)) (fibo (- n 2))))))
(vmeval 'vm '(fibo 10))
