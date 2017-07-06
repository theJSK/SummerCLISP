(format t "Enter ten integers separated by spaces, then press return.~%")
(setq myList '())

(loop for i from 1 to 10
   do (setq mylist (append mylist (list (read))))
   )
;(loop for j from 0 to 9
;   do (format t "~a " (nth j myList))
;     )

(defun recursive_check_list (myList)
   (cond
     ((null (cadr myList))
      (format t "List is in order."))
     ((> (car myList) (cadr myList))
      (format t "List is not in order.~%"))
     (T (recursive_check_list (rest myList)))
   )
  )

  
(recursive_check_list myList)

