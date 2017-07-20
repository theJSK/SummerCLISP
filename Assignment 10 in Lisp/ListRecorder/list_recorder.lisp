(format t "Enter ten integers separated by spaces, then press return.~%")
(setq mylist '())
(loop for i from 1 to 10
   do (setq mylist (append mylist (list (read))))
     )
;(format t "~a " (nth 3 myList))
(loop for j from 0 to 9
   do (format t "~a " (nth j mylist))
     )
    
