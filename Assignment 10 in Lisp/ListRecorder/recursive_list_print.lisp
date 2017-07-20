(format t "Enter ten integers separated by spaces, then press return.~%")
(setq mylist '())
;(setq firstNumb (read))
(setq listLength 0)

(defun recursive-list-print (listLength)
  (cond ((eq listLength 10)
	 (loop for i from 0 to 9
	    do (format t "~D " (nth i myList))
	      ))
	(T
	 (setq myList (append myList (list (read))))
	 (recursive-list-print (1+ listLength))
	 )
	)
  )

(recursive-list-print listLength)
