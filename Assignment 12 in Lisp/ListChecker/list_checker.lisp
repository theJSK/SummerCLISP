(format t "Enter ten integers separated by spaces, then press return!!!~%")
(setq mylist '())
(setq isInOrder 0)
(loop for i from 1 to 10
   do (setq mylist (append mylist (list (read))))
     )
(loop for i from 0 to 8
   do
     (cond
       ((>= (nth (+ 1 i) myList) (nth i myList))
	 (setq isInOrder 0))
	 (T (setq isInOrder 1) (return))
	 )
     )
(cond
  ((eq isInOrder 0) (format t "List is in order.~%"))
  (T (format t "List is not in order.~%"))
  )




