(format t "Enter ten integers separated by spaces, then press return!!!~%")
(setq mylist '())
(loop for i from 1 to 10
   do (setq mylist (append mylist (list (read))))
     )
(loop for j downfrom 9 to 0
   do (format t "~a " (nth j mylist))
     )
    
