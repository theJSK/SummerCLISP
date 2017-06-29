(format t "Enter ten integers separated by spaces, then press return.~%")
(setq mylist '())
(setq firstNumb (read))
(setq listLength 0)

(defun recursive-list-print (firstNumb listLength)
  
  (cond ((eq listLength 2) (format t "~D ~% " myList))
	(T
	 (setq myList (append myList (list (firstNumb))))
	 (recursive-list-print firstNumb (1+ listLength))
	 )
	)
  )

(recursive-list-print firstNumb listLength)
