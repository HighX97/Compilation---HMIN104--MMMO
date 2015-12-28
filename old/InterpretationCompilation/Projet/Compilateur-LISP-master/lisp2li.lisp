;analyse par cas
;lisp -> langage intermediaire
;je pense pas qu'elle soit vraiment complete mais elle fait deja beaucoup de trucs

;1. defun
;2. macro
;3. fs


;(load "C:/Users/Marama/workspace/IN108Project/lisp2li.lisp")
;(load "/auto_home/dvaret/workspace/LispProjet/lisp2li.lisp")


(defun lisp2li (expr env)
  ;(print :-------)
  ;(print expr)
  ;si expr est un atome
  (if (atom expr)
    ;alors on teste si c'est une constante
    (if (constantp expr)
      (list :const expr)
      ;ou une variable de l'environnment
      (let ((pos (position expr env)))
        (if pos
          (list :var (+ 1 pos))
          ;(warn "variable inconnue ~s" expr)
          (list :inc expr))))
    ;si expr n'est pas un atome
    (let ((fun (car expr)) (args (cdr expr)))
      (cond
        ;cas du defun
        ;j'associe la fonction a son nom dans la fonction lisp2li
        ;ca sert pour le mcall plus bas
        ((eq 'defun fun)
          (setf (get (first args) :defun)
            (list
              (length (second args))
              (first args) 
              (lisp2li (third args) (second args)))))
        ;du coup il faut redefinir le set dans la fonction
        ;deux formes :  setf / set-var
        ;(lisp2li '(defun test_set (x) (let ((a 0)) (setf a 1))) ())
        ((eq 'setf fun)
          (setlidefun expr env))
        ;gestion de la macro loop (?)
        ((eq 'loop fun)
          (let ((var (list (list (cadr args) (cadddr args)))))
          (list
            :let
            1
            (letlisp2li var (addtoenv var env))
            (list (looptoli (cdr args) (addtoenv var env)))              
            )))
        ;gestion des macros
        ;macroexpand d'un case
        ;let : ajout d'une variable locale a l'environnement + cond ("macroexpand" plus tard)
        ((macro-function fun)
            (lisp2li (macroexpand-1 expr) env))
        ;la il y a les formes speciales
        ;la fonction speciale-form-p me donnait des trucs bizarres
        ;alors j'ai separe tous les cas
        ;il manque surement des formes speciales
        ((eq 'quote fun)
          (list :const (first args)))
        ((eq 'if fun)
          (cons
            :if
            (maplisp2li args env)))
        ((eq 'progn fun)
          (list
            :progn
            (maplisp2li args env)))
        ((eq 'let fun)
          ;(:let nb_args args)
          (list
            :let
            ;(print (car args))
            ;(print env)
            (length (car args))
            (letlisp2li (car args) (addtoenv (car args) env))
            ;(maplisp2li (car args) (car args))
            (maplisp2li (rest args) (addtoenv (car args) env))
            ))
        ((eq 'let* fun)
          ;(:let nb_args args)
          ;(lisp2li '(defun test_let (x) (let ((n 10)) (+ x n))) ())
          ;(comp '(defun test_let (x) (let ((n 10)) (+ x n))))
          (list
            :let
            (length (car args))
            ;(print (car args))
            (letlisp2li (car args))
            ;(lisp2li (car args))
            (maplisp2li (rest args) (addtoenv (car args) env))))
        ;gestion du reste des appels de fonctions
        ((not (null (getlidefun fun)))
          ;fonction utilisateur definie dans l'environnement
          (list
            :mcall
            fun
            (maplisp2li args env)))
        ((not (fboundp fun))
          ;fonction inconnue ou premiere apparition de la fonction dans l'environnement
          (list
            :unknown
            (cons fun args) env))
        ((fboundp fun)
          ;fonction definie dans clisp
          (list
            :call
            fun
            (maplisp2li args env)
            ))))))

;(lisp2li '(defun fibo200a (n x y) (if (<= n 0) y (fibo200a (- n 1) y (+ x y)))) ())
;(lisp2li '(defun lvm (vm code) (loop for instr in code do (+ 1 vm))) ())
;(lisp2li '(defun aaa (x y z) (let ((a 5) (b 10)) (+ a x) (+ y b))) ())

;ne gere que le cas "loop for"
;loop for instr in code do (print instr)
;(defun loop_test (li n) macroexpand-1 (loop for cel in li do (+ 1 n)))))
;
(defun looptoli (lexpr env)
  (let ((val (caddr lexpr)) (args (cddddr lexpr)))
    ;(print args)
    (list
      :loop      
      (lisp2li val env)
      (random (random 1000000))
      (lisp2li (car args) env)
      (lisp2li (append (list 'setf (car lexpr) (list 'cdr (car lexpr)))) env ))))

(defun addtoenv (lexpr env)
  ;(print :addtoenv)
  ;(print lexpr)
  (let ((expr (car lexpr)))
          (if (atom lexpr)
            env
            (addtoenv (cdr lexpr) (append env (list (car expr)))))))
            

(defun letlisp2li (lexpr env)
  ;(print env)
  ;(print lexpr)
  (let* ((expr (car lexpr)) (pos (position (car expr) env)))
  (if (atom lexpr)
    ()
    (cons
      (list 
        :let-var
        (+ 1 pos)
        (lisp2li (cadr expr) env))
        ;(list (car expr) (cadr expr)))
      (letlisp2li (cdr lexpr) env)))))


;les petits tests sympatiques
;(lisp2li '(if (< a 0) 1 0) '(a))
;(lisp2li '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1))))) ())
;(lisp2li '(defun fibo (n) (if (<= n 0) 1 (+ (fibo (- n 1)) (fibo (- n 2))))) ())
;(lisp2li '(defun cc (a) (cond ((< a 1) 0) ((> a 1) 1))) '(a))
;(lisp2li '(defun fibonacci (n) (cond ((> n 1) (+ (fibonacci (- n 1)) (fibonacci (- n 2)))) ((= n 1) 1) ((= n 0) 0))) ())
;(lisp2li '(cond ((< a 1) 0) ((> a 1) 1)) '(a))
;(lisp2li '(cond ((< 0 1) 0) ((> 0 1) 1)) ())
;(butlast (macroexpand '(cond ((< a 1) 0) ((> a 1) 1))))
;(lisp2li '(defun t27 (n) (if (<= n 0) (+ n n) (* (t27 (- n 1)) n))) ())

;rien de special, c'est la fonction du prof
(defun maplisp2li (lexpr env)
  (if (atom lexpr)
    ()
    (cons
      (lisp2li (first lexpr) env)
      (maplisp2li (rest lexpr) env))))

;verifie si fun est associe a quelque chose dans l'environnement
(defun getlidefun (fun)
  (get fun :defun))

;redefinition du set
(defun setlidefun (expr env)
  (if (symbolp (second expr))
      (list
       :set-var
       (lisp2li (second expr) env)
       (lisp2li (third expr) env))
      (list
       :setf
       (lisp2li (second expr) env))))
