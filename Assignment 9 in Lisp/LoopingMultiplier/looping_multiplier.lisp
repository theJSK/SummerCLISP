(format t "Enter two numbers and press return.~%")
(setq product 0)
(setq firstNumb (read))
(setq secondNumb (read))
(loop for secondNumb from 1 to secondNumb
     do (setq product (+ product firstNumb))
     )	 
(format t "~D~%" product)


