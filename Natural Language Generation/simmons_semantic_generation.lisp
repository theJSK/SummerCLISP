;; This code is based on Simmons and Slocum 1972

;; GRAMMAR NETWORK
;; There are two different grammar networks in the paper.
;; This is the grammar network from the later part of the paper.
;; Here it is on the property list.
;; Most of the rules are "binary" (e.g. agt pred) in that they emit a grammar terminal or non-terminal
;; And then visit another state.
;; Some rules are "unary", though.  Unary rules (e.g. np0) are identified by having a single item on the list

(setf (get 's 'paths) '( (agt pred) (dat pred) ) )
(setf (get 'agt 'paths) '( (np0) ) )
(setf (get 'dat 'paths) '( (np0) ) )
(setf (get 'np0 'paths) '( (pobj np1) (np1) ) )
(setf (get 'np1 'paths) '( (nbr np2) ) )
(setf (get 'np2 'paths) '( (det np3) (np3) (mod np3) ) )
(setf (get 'np3 'paths) '( (mod np3) (np4) ) )
(setf (get 'np4 'paths) '( (ns t) ) )
(setf (get 'pred 'paths) '( (man vp0) (vp0) ) )
(setf (get 'vp0 'paths) '( (aux vp1) ) )
(setf (get 'vp1 'paths) '( (vs vp2) ) )
(setf (get 'vp2 'paths) '( (obj vp3) (vp3) ) )
(setf (get 'vp3 'paths) '( (loc vp4) (dat vp4) (inst vp4) (vp4) ) )
(setf (get 'vp4 'paths) '( (t) ) )
(setf (get 'obj 'paths) '( (pobj np1) (nbr np2) (agt pred) ) )
(setf (get 'pobj 'terminal) 't)
(setf (get 'nbr 'terminal) 't)
(setf (get 'det 'terminal) 't)
(setf (get 'ns 'terminal) 't)
(setf (get 'man 'terminal) 't)
(setf (get 'aux 'terminal) 't)
(setf (get 'vs 'terminal) 't)


;; SEMANTIC NETWORK
;; You'll need to reset these property lists every time you generate.
;; leaving off the reverse (*) links.
(setf (get 'c3 'tok) 'l3)
;; below the paper had "tim" instead of aux.  this causes trouble because the grammar given in the paper only allows aux as the transition.  I'm making it aux.
(setf (get 'c3 'aux) 'prog-past)
(setf (get 'c3 'agt) 'c4)
(setf (get 'c3 'obj) 'c7)
;; word class relations aren't in table III in the paper
;; so adding vs here
(setf (get 'c3 'vs) '())
(setf (get 'c4 'tok) 'l4)
(setf (get 'c4 'nbr) 'sing)
;; so adding ns here
(setf (get 'c4 'ns) '())
(setf (get 'c5 'tok) 'l5)
(setf (get 'c7 'tok) 'l7)
(setf (get 'c7 'det) 'indef)
(setf (get 'c7 'nbr) 'sing)
(setf (get 'c7 'pobj) 'c5)
(setf (get 'c7 'ns) '())
(setf (get 'l3 'pi) 'wrestle)
; comment
(setf (get 'l4 'pi) 'mary)
(setf (get 'l5 'pi) 'with)
(setf (get 'l7 'pi) 'bottle)
(setf (get 'l3 'terminal) 't)
(setf (get 'l4 'terminal) 't)
(setf (get 'l5 'terminal) 't)
(setf (get 'l7 'terminal) 't)

;; GENERATION FUNCTIONS

;; NOTE: The generation process functions permanently change the property lists of the semantic network, so you'll need to reset the semantic network property lists before generating again in the same LISP image.  Suggest making the reset a function that you can call.

;; Function corresponding to non-terminals
;; Right now many of these functions usually just grab the print image of the word
;; or do nothing.  In the future they may have to handle tense of verbs or singular/plural for
;; nouns, adding determiners to noun phrases, etc.
(defun nbr (structure)
  (setf (get structure 'ns) (append  (get structure 'ns) (list (get (get structure 'tok) 'pi)) ))
  nil
  )

(defun ns (structure)
  (get structure 'ns)
  )

(defun aux (structure)
  (setf (get structure 'vs) (append (list (get (get structure 'tok) 'pi)) (get structure 'vs)))
  nil
  )

(defun vs (structure)
  (get structure 'vs)
  )

(defun pobj (structure)
  (setf (get structure 'ns) (append (list (get (get (get structure 'pobj) 'tok) 'pi)) (get structure 'ns)))
  nil
  )

(defun det (structure) nil )

(defun gen (structure)
  ;; don't really need this first line at the moment, since i'm outputting a print image below
  (cond ((setq j (get structure 'terminal)) (print "terminal node. exiting.") (list j))
	((null (setq j (get structure 'lab))) (print "no structure label. exiting.") nil)
	
	;; In this hacked version, won't check to see if label is in the grammar
	;; if you get this far, you have a match between grammar and structure!
	;; now i need to check all rules until I have a match
	;; j is the grammar label on the semantic structure.  Get the paths from that
        ;; node in the grammar and see if any match the semantic structure
	;; if there is no match, return nil
	((null (setq rule (get-matching-rule structure (get j 'paths))))  (format t "the structure ~a had nothing matching paths in the grammar state ~a.  exiting.~%" structure j) nil)
	;; if there is a match now rule is set to the grammar rule
	;; if it's a unary rule, just relabel the semantic structure to that node in the grammar
	;; and call gen recursively
	
	((= 1 (length rule)) (setf (get structure 'lab) (car rule)) (format t "Match was a unary rule, so relabeling structure ~a to ~a and calling gen on it recursively.~%" structure (car rule)) (gen structure))
	;; if it's not a unary rule, then the grammar state symbol is the second element of rule
	;; set k to the semantic structure node that we're connected to
	(T (print "made it to non unary rule code")
	   (format t "going to generate again based on matching between structure ~a and grammar path ~a.~%"
		   structure rule)
	   (setq k (get structure (car rule)))
	   (format t "k was set to ~a. ~%" k)
	   ;; relabel the semantic structure with the grammar node symbol at the end of the
	   ;; transition
	   (setf (get structure 'lab) (cadr rule))
	   (format t "structure node ~a was relabeled to ~a.~%" structure (cadr rule)) 
	   ;; if this new semantic structure k, is a terminal node, the simmons paper recursively calls
	   ;; gen but first apply the function corresponding to the structure label to the structure
	   ;; I am instead just going to return a list with the print image symbol.
	   (cond
	     ;; for now just print the grammar symbol instead of the print image.
	     ((get (car rule) 'terminal)
	      (format t "~a is a terminal node in the grammar so we're printing it but calling gen recursively on structure ~a.~%" (car rule) structure)
	      (append (apply (car rule) (list structure)) (gen structure)) )

	     ;; Not a terminal, so we're actually going to connect to a semantic structure node
	     ;; label the new structure that we're visiting after the grammar non-terminal
	     ;; what I should probably do is build a couple of "visitor" functions
	     (T (setf (get k 'lab) (car rule))
		(format t "labeled structure node ~a to ~a.~%" k (car rule))
		;; normally the simmons version of the code deletes the connection from the
		;; semantic network, and calls apply with the name of the relation as a function
		;; i won't do this for now, but may later.
		(format t "now calling gen again with structure nodes ~a and ~a.~%" k structure)
		(append (gen k) (gen structure))
		)
	     )
	   )
	)
  )


;; return a matching rule or nil if there was none
(defun get-matching-rule (structure rule-list)
  (cond ((null rule-list) nil)
	;; if the first rule in the list is a list with a single element, then it's a
	;; "unary" rule.  It always matches.  Return it.
	((null (cdar rule-list)) (car rule-list))
	;; not a unary rule, so see if the syntactic arc matches a relation in the semantic structure
	;; if it does, it's your match so return it.
	((get structure (caar rule-list)) (car rule-list))
	;; the car isn't a match.  Recursively call
	(T (get-matching-rule structure (cdr rule-list)))
	)
  )

;; Need to label the semantic network with the node in the grammar network where
;; you want generation to start.
(setf (get 'c3 'lab) 's)

;; Evaluate this to generate starting at semantic network node c3.
(print (gen 'c3))

;; Some tests of functions
;; (apply 'nbr (list 'c3))
;; (get-matching-rule 'c3  '((dat pred) (agt pred) ) )
 
