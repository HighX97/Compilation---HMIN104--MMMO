;(load "C:/Users/Marama/workspace/IN108Project/run.lisp")
;(load "/auto_home/dvaret/workspace/LispProjet/run.lisp")
;(require "lisp2li.lisp")
;(require "litoasm.lisp")
(require "evalli.lisp")
(require "makevm.lisp")

(makevm 'vm)
;(vmeval 'vm '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1))))))
;(vmeval 'vm '(charge_vm 'vm '((:const 1) (:const 2) (:call +))))

;(vmeval 'vm '(defun get-CC (vm)
 ; (aref (get vm 'pile) 4)))

;(vmeval 'vm '(defun set-case (vm index val)	
;  (setf (aref (get-pile vm) index) val)))
  
;(vmeval 'vm '(defun get-pile (vm)	
 ; (get vm 'pile)))
  
;(vmeval 'vm '(defun vmdecr (vm dest)
 ; (let ((v1 (get-case vm dest)))
   ; (set-case vm dest (- v1 1)))))
 
;(vmeval 'vm '(defun get-case (vm index)	
 ; (aref (get-pile vm) index)))

;(vmeval 'vm '(defun charge_vm_acc (vm code ghtrr ghtra lhtrr lhtra)
 ; (if (atom code)	
  ;  ()
   ; (let ((instr (first code)))
    ;  (cond
     ;   ((eq (first instr) :label)
      ;    (let ((addr (get-CC vm)) (label (second instr)))
       ;     (if (symbolp (second instr))
        ;      (progn
         ;       (load-label addr label ghtrr ghtra)
          ;      (charge_vm_acc vm (cdr code) ghtrr ghtra lhtrr lhtra))
           ;   (progn
            ;    (load-label addr label lhtrr lhtra)
             ;   (charge_vm_acc vm (cdr code) ghtrr ghtra lhtrr lhtra))
              ;)))	
       ; (T
        ;  (set-case vm (get-CC vm) instr)
         ; (vmdecr vm 4)
          ;(charge_vm_acc vm (cdr code) ghtrr ghtra lhtrr lhtra)
          ;))))))

;(vmeval 'vm '(defun charge_vm (vm code)
 ;              (let ((ghtrr (get vm 'etiquettes_resolues))
  ;                    (ghtra (get vm 'references_avant))
   ;                   (lhtrr (make-hash-table))
    ;                  (lhtra (make-hash-table)))
     ;            (charge_vm_acc vm code ghtrr ghtra lhtrr lhtra))))


;(vmeval 'vm '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1))))))
;(vmeval 'vm '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))))
;(vmeval 'vm '(defun fibo (n) (cond ((= n 0) 0) ((= n 1) 1) (T (+ (fibo (- n 1)) (fibo (- n 2)))))))

;(defun fibo (n) (cond ((= n 0) 0) ((= n 1) 1) (T (+ (fibo (- n 1)) (fibo (- n 2))))))

;;;TIME C-LISP

;(fibo 30)
;real time : 2.6491516 sec. - run time : 2.652017 sec.
;space : 0 Bytes

;(fiboterm 930)
;real time : 0.0030002 sec. - run time : 0.0 sec.
;space :  46,432 Bytes

;;;TIME VM-EVAL

; --FACT--

;(vmeval 'vm '(defun fact (n) (if (<= n 0) 1 (* n (fact (- n 1))))))
;(vmeval 'vm '(fact 100))
;real time = 0.0640037 sec. - run time = 0.06224004 sec.
;space = 4140 Bytes

; --FIBO-TERM--

;(vmeval 'vm '(defun fibo2 (n x y) (if (<= n 0) y (fibo2 (- n 1) y (+ x y)))))
;(vmeval 'vm '(defun fiboterm (n) (fibo2 n 1 0)))

;(vmeval 'vm '(fiboterm 100))
;real time = 0.0720041 sec. - run time = 0.0780005 sec.
;space = 1376 Bytes

;(vmeval 'vm '(fiboterm 1000)) 
; - possibilite de depasser la limite de meval 
; - necessite une machine plus grande que celle par defaut
;real time = 0.4050232 sec. - run time = 0.4056026 sec.
;space = 53,360 Bytes


;;;TIME EVAL-LI

; --FIBO--

;(meval '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))))
;(meval '(fibo 20))
;real time : 1.6970972 sec. - run time : 1.7004108 sec. 
;space : 4,202,976 Bytes
;GC : 7, GC time : 0.0780005 sec.

;(meval '(fibo 25))
;real time : 11.289646 sec. - run time : 11.247672 sec.
;space : 46,614,624 Bytes
;GC : 78, GC time : 0.5148033 sec.

;(meval '(fibo 30))
;real time : 121.29094 sec. - run time : 120.15197 sec.
;space : 516,967,008 Bytes
;GC : 865, GC time : 5.3664346 sec.


; --FIBO-TERM--

;(meval '(defun fibo2 (n x y) (if (<= n 0) y (fibo2 (- n 1) y (+ x y)))))
;(meval '(defun fiboterm (n) (fibo2 n 1 0)))
;(meval '(fiboterm 30))
;real time : 0.0200011 sec. - run time : 0.0312002 sec.
;space :  9960 Bytes
;GC : 1, GC time : 0.0156001 sec.

;(meval '(fiboterm 930)) - stack overflow au dela
;real time : 0.1830104 sec. - run time : 0.1404009 sec.
;space :  351,592 Bytes
;GC : 1, GC time : 0.0156001 sec.

