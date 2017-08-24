;; 2016-2017
;; Jamie Macbeth and Jung Soo Kim with help from Zack Ervin and Ting Li
;; Based on Simmons and Slocum 1972

;; This generate function and grammar network are from the early part
;; of the paper, They can be used to generate random sentences based
;; on the word sets below.  We've taken liberties to extend symbol
;; names (for example, np->noun-phrase) for clarity.

(defun get-random-nth (x) (nth (random (length x)) x))

(defun generate-from-list (x)
  (append (generate (car x)) (generate (cdr x))))

;; Changed from a 50/50 coin flip to 33.3%
(defun stop-at-terminal (x)
  (and (member x terminals) (not (= (random 2) 0))))

(defun generate-word (x) 
  (list (get-random-nth (get x 'words))))

(defun generate-from-path (x) 
  (generate (get-random-nth (get x 'paths))))

;; Corresponds loosely to the code in Table 1 in Simmons and Slocum 1972
(defun generate (state)
  (cond ((null state) nil)
	((listp state) (generate-from-list state))
	((stop-at-terminal state) nil)
	((get state 'words) (generate-word state))
	(T (generate-from-path state))))

(setf terminals '(q4 q7 q9 q6 tt))

;; grammar-paths is a list where each item provides the name of a state
;; in the grammar and the paths leading from it to other states.
;; For example "(q1 (verb-phrase tt) (auxiliary-verb q10) )" means that there are two
;; edges from q1, one edge is labeled verb-phrase and leads to state tt, the other edge
;; is labeled auxiliary-verb and leads to state q10.
;; These correspond to Figure 1 in Simmons and Slocum 1972
(setf grammar-paths '(
		      ;; The S (start) state machine
		      (start (noun-phrase q1) (auxiliary-verb q2) )
		      (q1 (verb-phrase tt) (auxiliary-verb q10) )
		      (q10 (verb-phrase tt) )
		      (tt nil)
		      ;; The NP (noun-phrase) state machine
		      (noun-phrase (determiner q3) (adjective q3) (noun q4) )
		      (q2 (noun-phrase q10) )
		      (q3 (adjective q3) (noun q4) )
		      (q4 (prepositional-phrase q4) )
		      ;; The VP (verb-phrase) state machine
		      (verb-phrase (verb q6) )
		      (q6 (noun-phrase q7) )		      
		      (q7 (prepositional-phrase q7) )
		      ;; The PP (prepositional-phrase) state machine
		      (prepositional-phrase (preposition q8) )
		      (q8 (noun-phrase q9) )
		      (q9 nil)
		      )
      )


;; word-sets is a list where each element is a list consisting of a terminal symbol name,
;; such as "adjective" and, following that, the rest of the list are the words in that
;; category.
(setf word-sets
      '((adjective old little red rickety young blue yellow big straight)
	(determiner a the another)
	(noun man woman cow horse road wagon market wheel)
	(auxiliary-verb will did)
	(verb break pull turn fix push)
	(preposition to on in from above below)
	)
      )

;; another word set
(setf fast-food-word-sets
      '((adjective juicy delicious creamy ice-cold hot chocolate vanilla
	 strawberry crisp fried grilled cheesy small medium large extra fresh new)
	(determiner a the another)
	(noun customer employee cook manager coupon burger cheeseburger tortilla
	 beef bun lettuce cheese onion pickle
	 coca-cola pepsi sprite soda meal shake ketchup mustard hotdog relish mayo)
	(auxiliary-verb should will did)
	(verb eat cook make grill buy bite sip drink try pour spill drop put)
	(preposition on in with without under over before)
	)
      )


;; another word set
(setf star-trek-word-sets
      '((adjective klingon vulcan romulan federation starfleet bold warpspeed planetary cosmic exploding )
	(determiner a the another)
	(noun earth planet asteroid star space voyage kirk spock uhuru scotty sulu shield starship phaser torpedo transporter engine quadrant nebula captain )
	(auxiliary-verb should will did)
	(verb fire transport engage plot turn lead go orbit explode)
	(preposition to on in from before beyond)
	)
      )




;; another word set
(setf sports-word-sets
      '((adjective fast quick slow big flagrant )
	(determiner a the another)
	(noun danica nadal lebron serena ball racket hoop net serve track car clock quarter game win rebound court referee foul coach team uniform)
	(auxiliary-verb should will did)
	(verb hit shoot drive throw play pass win lose crash eject)
	(preposition to on in from for)
	)
      )



(defun make-property-lists (x property-name)
  (mapcar  ; call the following lambda on each member of the list

   ;; This lambda takes a list as an argument The first element (or car) of the
   ;; list is the symbol whose property list we want to store under.
   ;; property-name is the property of that symbol where we want to
   ;; store the value (e.g. 'paths).  The rest (or "cdr") of the list
   ;; is the value that we want to put as that property.
   
   #'(lambda (y) (setf (get (car y) property-name) (cdr y)))

   ;; and mapcar is called on this argument, x.  See grammar-paths or
   ;; word-sets for examples.
   
   x)
  )

;; These will update the property lists for the grammar and word sets for
;; terminal symbols in the grammar.  Some alternate word sets are commented
;; out.
(make-property-lists grammar-paths 'paths)
(make-property-lists word-sets 'words)
;;(make-property-lists fast-food-word-sets 'words)
;;(make-property-lists star-trek-word-sets 'words)
;;(make-property-lists sports-word-sets 'words)


(print (generate 'start))



