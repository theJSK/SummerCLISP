(format t "Enter an integer and press return.~%")
   (setq month (read))
   (cond
     ((eq month 1) (format t "~a" "January"))
     ((eq month 2) (format t "~a" "February"))
     ((eq month 3) (format t "~a" "March"))
     ((eq month 4) (format t "~a" "April"))
     ((eq month 5) (format t "~a" "May"))
     ((eq month 6) (format t "~a" "June"))
     ((eq month 7) (format t "~a" "July"))
     ((eq month 8) (format t "~a" "August"))
     ((eq month 9) (format t "~a" "September"))
     ((eq month 10) (format t "~a" "October"))
     ((eq month 11) (format t "~a" "November"))
     ((eq month 12) (format t "~a" "December"))
     (T (format t "~a" "Invalid Month"))
     )



