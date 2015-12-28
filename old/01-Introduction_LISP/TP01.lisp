(defun f (x) (+ 3 x)) 

(defun g (v) (* 5 (f (+ v 2)))) 

(defun facto (n)
    (if (< n 1)
 	       1
    	   (* n (facto (- n 1)))))    	   
  	   
(defun fibona (n)
    (if (< n 2)
			1
			(+ (fibo (- n 1)) (fibo (- n 2)))))

;fonctions sur les listes plates			
(defun mymember (x l)
	(if (atom l)
		l
		(if (eq (car l) x)
			l
			(mymember x (cdr l)))))

(defun mylength (l)
	(if (atom l)
		0
		(+ 1 (mylength (cdr l)))))
		
(defun last (l)
	(if (atom l)
		l
		(if (atom (cdr l))
			(car l)
			(last (cdr l)))))

(defun makelist (n)
	(if (< n 0)
		()
		(cons n (makelist (- n 1)))))    	   
		
(defun copylist (l)
	(if (atom l)
		l
		(if (atom (cdr l))
			(cdr l)
			(cons (car l) (copylist (cdr l))))))
			
(defun removeOcc (x l)
	(if (atom l)
		l
		(if (= x (car l))
			(remove x (cdr l))
			(cons (car l) (remove x (cdr l))))))
			
(defun removeLOcc (x l)
	(if (atom l)
		l
		(if (= x (car l))
			(cdr l)
			(cons (car l) (remove x (cdr l))))))	
			
(defun myappend (l1 l2)
	(if (atom l1)
		l2
		(if (atom l2)
			l1
			(cons (car l1) (myappend (cdr l1) l2)))))

(defun myadjoin (x l)
  (if (atom l)
      (cons x '())
    (if (eql x (car l))
	l
      (cons (car l) (myadjoin x (cdr l))))))

(defun match-list (l1 l2)
  (if (and (atom l1) (atom l2))
      ()
    (if (atom l1)
	(cons (cons (car l2) ()) (match-list l1 (cdr l2)))
      (if (atom l2)
	  (cons (cons (car l1) ()) (match-list (cdr l1) l2))
	(cons (cons (car l1) (car l2)) (match-list (cdr l1) (cdr l2)))))))

;fonction sur les arbres binaires

(defun treesize (tree)
  (if (atom tree)
      0
    (+ 1 (treesize (car tree)) (treesize (cdr tree)))))

(defun copytree (tree)
  (if (atom tree)
      tree
    (if (atom (cdr tree))
      (car tree)
    (cons (copytree (car tree)) (copytree (cdr tree))))))

(defun mysubst (x y tree)
  (if (atom tree)
      tree
    (if (eql (car tree) x)
	(cons y (mysubst x y (cdr tree)))
      (cons (car tree) (mysubst x y (cdr tree))))))

(defun tree-leaves (tree)
  (if (atom tree)
      tree
    (if (atom (cdr tree))
	(car tree)
      (cons (car tree) (tree-leaves (cdr tree))))))


(defun miroir (tree)
  (if (atom tree)
      tree
    (cons (miroir (cdr tree)) (miroir (car tree)))))

(defun depth (tree)
  (if (atom tree)
      0
    (+ 1 (depth (cdr tree)))))

(defun match-tree (t1 t2)
  (if (and (atom t1) (atom t2))
      (cons (cons t1 t2) () )
    (if (atom t1)
	(cons (cons (car t2) ()) (match-tree t1 (cdr t2)))
      (if (atom t2)
	  (cons (cons (car t1) ()) (match-tree (cdr t1) t2))
	(append (match-tree (car t1) (car t2)) (match-tree (cdr t1) (cdr t2)))))))

;better match-tree - append -> glue
;à terminer
(defun match-tree-t (t1 t2 t3)
  (if (and (atom t1) (atom t2))
      (cons (cons t1 t2) () )
    (if (atom t1)
	(cons (cons (car t2) ()) (match-tree-t t1 (cdr t2) t3))
      (if (atom t2)
	  (cons (cons (car t1) ()) (match-tree-t (cdr t1) t2 t3))
	((match-tree-t (car t1) (car t2) (match-tree-t (cdr t1) (cdr t2) t3))))))))



    
