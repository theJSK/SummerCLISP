(format t "Enter an integer and press return.~%")
   (setq firstInteger (read))
   (loop for i from 1 to firstInteger
	do (format t "~a" "Lisp")
	)


