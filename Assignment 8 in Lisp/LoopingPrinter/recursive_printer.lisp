(format t "Enter an integer and press return.~%")
(setq firstInteger (read))
(defun recursive-print (x)
  (cond ((eq x 0))
	(T (format t "~a" "Lisp") (recursive-print (1- x))
	)
  )
)
(recursive-print firstInteger)




