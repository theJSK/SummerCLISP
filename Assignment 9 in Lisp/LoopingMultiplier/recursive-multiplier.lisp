(format t "Enter two integers and press return.~%")
(setq product 0)
(setq firstNumb (read))
(setq secondNumb (read))
(defun recursive-mult (firstNumb secondNumb)
   (cond ((eq secondNumb 0) (format t "~D ~%" product))
      (T
	(setq product (+ product firstNumb))
	(recursive-mult firstNumb (1- secondNumb))
	)
	)
)
	
(recursive-mult firstNumb secondNumb)
		
