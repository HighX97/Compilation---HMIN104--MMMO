(defun fact (n)
    (
	    if (< n 1)
 	       1
    	   (* n (fact (- n 1)))))
        
(defun fibo (n)
    (
		if (< n 2)
			1
			(+ (fibo (- n 1)) (fibo (- n 2)))))
