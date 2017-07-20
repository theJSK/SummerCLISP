(format t "Enter two integers. Press return after each integer. ~%")
	   (setq firstInteger (read))
	   (setq secondInteger (read))
	   (format t "Enter an operation (+,-,*,/, or %). Then press return.~%")
	   (setq operation (read))
	   (cond
	     ((eq operation '+)(format t "~D" (+ firstInteger secondInteger)))
	     ((eq operation '-)(format t "~D" (- firstInteger secondInteger)))
	     ((eq operation '*)(format t "~D" (* firstInteger secondInteger)))
	     ((eq operation '/)(format t "~D" (floor firstInteger secondInteger)))
	     ((eq operation '%)(format t "~D" (mod firstInteger secondInteger)))
	     )



