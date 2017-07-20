;; Based on Simmons and Slocum 1972
;; This gen function and grammar network are from the early part of the paper,
;; They can be used to generate random sentences based on the word sets below.

(defun get-random-nth (x) (nth (random (length x)) x))

(defun generate-from-list (x)
  (append (gen (car x)) (gen (cdr x))))

(defun stop-at-terminal (x)
  (and (get x 'terminal) (= (random 2) 0)))

(defun generate-word (x) 
  (list (get-random-nth (get x 'words))))

(defun generate-from-path (x) 
  (gen (get-random-nth (get x 'paths))))
       
(defun gen (s)
  (cond ((null s) nil)
	((listp s) (generate-from-list s))
	((stop-at-terminal s) nil)
	((get s 'words) (generate-word s))
	(T (generate-from-path s))))

(setf (get 'q10 'paths) '((vp tt)))
(setf (get 'q1 'paths) '((vp tt)(aux q10)))
(setf (get 'q2 'paths) '((np q10)))
(setf (get 'q4 'paths) '((pp q4)))
(setf (get 'q3 'paths) '((adj q3) (n q4)))
(setf (get 'q7 'paths) '((pp q7)))
(setf (get 'q6 'paths) '((np q7)))
(setf (get 'q9 'paths) '(nil))
(setf (get 'q8 'paths) '((np q9)))
(setf (get 'np 'paths) '((det q3) (adj q3) (n q4)))
(setf (get 'pp 'paths) '((prep q8)))
(setf (get 'vp 'paths) '((v q6)))
(setf (get 'tt 'paths) '(nil))
(setf (get 's 'paths) '((np q1)(aux q2)))
(setf (get 'q4 'terminal) T)
(setf (get 'q7 'terminal) T)
(setf (get 'q9 'terminal) T)
(setf (get 'q6 'terminal) T)
(setf (get 'tt 'terminal) T)

(setf (get 'adj 'words) '(old little red rickety young blue yellow big straight))
(setf (get 'det 'words) '(a the another))
(setf (get 'n 'words) '(man woman cow horse road wagon market wheel))
(setf (get 'aux 'words) '(will did))
(setf (get 'v 'words) '(break pull turn fix push))
(setf (get 'prep 'words) '(to on in from above below))

(gen 's)
