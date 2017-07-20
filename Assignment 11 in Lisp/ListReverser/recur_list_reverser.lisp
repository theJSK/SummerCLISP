(format t "Enter ten integers separated by spaces, then press return.~%")
(setq mylist '())
;(setq firstNumb (read))
(setq listLength 0)

(defun recursive-list-reverse (listLength)
  (cond ((eq listLength 10)
	 (loop for i downfrom 9 to 0
	    do (format t "~D " (nth i myList))
	      ))
	(T
	 (setq myList (append myList (list (read))))
	 (recursive-list-reverse (1+ listLength))
	 )
	)
  )

(recursive-list-reverse listLength)
