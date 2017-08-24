;; This code is based on Simmons and Slocum 1972
;; It generates natural language based on an english grammar and
;; a semantic network.
;; Just load everything directly on the property list in one shot

(defun reset-property-list ()
  (mapcar

   ;; Important!  The lambda expression needs to use copy-list,
   ;; because otherwise subsequent uses just reuse the same memory
   ;; as in the structure below, and you'll see JOHN JOHN SAW SAW ...
   #'(lambda (x) (setf (symbol-plist (first x)) (copy-list (second x))))

   '(
     ;; SEMANTIC NETWORK
     
     ;; The paper had TIM instead of AUX.  This causes trouble
     ;; because the grammar given in the paper only allows AUX as the
     ;; transition.  I'm making it AUX.  Word class relations aren't
     ;; in table III in the paper so adding VS NS and others here.
     ;; Just storing past tense and progressive tense verbs instead of
     ;; having code to construct them.  Making John AGT instead of
     ;; DAT.  See notes for further explanation.  Leaving off the reverse (*) links.

     ;; each property list is a set of pairs, e.g. tok l1 sets the property
     ;; "tok" to be "l1", then the property "aux" gets the value "past", etc.
     
     (c1 (tok l1 aux past agt c2 obj c3 vs () )) ;; see
     (c2 (tok l2 nbr sing ns () ) ) ;; John
     (c3 (tok l3 aux prog agt c4 obj c7 loc c8 vs () )) ;; wrestle 
     (c4 (tok l4 nbr sing ns ()) ) ;; Mary
     (c5 (tok l5) ) ;; with
     (c6 (tok l6) ) ;; at
     (c7 (tok l7 det indef nbr sing pobj c5 ns () ) ) ;; bottle
     (c8 (tok l8 nbr sing det def pobj c6 assoc c9 ns () ) ) ;; bar
     (c9 (tok l9 nbr sing ns () ) ) ;; liquor
     (l1 (pi see past saw terminal t) )
     (l2 (pi john terminal t) )
     (l3 (pi wrestle past wrestled prog wrestling terminal t) )
     (l4 (pi mary terminal t) )
     (l5 (pi with terminal t) )
     (l6 (pi at terminal t) )
     (l7 (pi bottle terminal t) )
     (l8 (pi bar terminal t) )
     (l9 (pi liquor terminal t) )

     ;; GRAMMAR NETWORK

     ;; Here the grammar, paths and terminal properties (when
     ;; applicable).  There are two different grammar networks in the
     ;; paper.  This is the grammar network from the later part of the
     ;; paper.  Most of the rules are "binary" (e.g. agt pred) in that
     ;; they emit a grammar terminal or non-terminal And then visit
     ;; another state.  Some rules are "unary", though.  Unary rules
     ;; (e.g. np0) are identified by having a single item on the list.
     ;; The grammar rewrite rules in Fig 4(b) and the triples in
     ;; Fig. 3.  have a dative (DAT) at the end of the PRED
     ;; generation, but for me, this generated extra copies of "john"
     ;; at the end.  For each grammar symbol, each paths property is a
     ;; list of lists.
     
     (s (paths ((agt pred) (dat pred))) )
     (agt (paths ((np0))))
     (dat (paths ((np0))))
     (np0 (paths ((pobj np1) (np1))))
     (np1 (paths ((nbr np2))))
     (np2 (paths ((det np3) (np3) (mod np3))))
     (np3 (paths ((mod np3) (np4))))
     (np4 (paths ((ns t))))
     (pred (paths ((man vp0) (vp0))))
     (vp0 (paths ((aux vp1))))
     (vp1 (paths ((vs vp2))))
     (vp2 (paths ((obj vp3) (vp3))))
     (vp3 (paths ((loc vp4) (dat vp4) (inst vp4) (vp4))))
     (vp4 (paths ((t))))
     (obj (paths ((pobj np1) (nbr np2) (agt pred))))
     ;; added this to grammar, since it wasn't in the paper.  location, "loc" acts like an object or
     ;; indirect object, so just have an unconditional transition to np0, just like with AGT, DAT
     (loc (paths ((np0))))
     ;; terminal symbols in the grammar, maybe just having paths be nil is another option
     (pobj (terminal t))
     (nbr (terminal t ))
     (det (terminal t))
     (ns (terminal t))
     (man (terminal t))
     (aux (terminal t))
     (vs (terminal t))
     )
   )
  )

 

;; GENERATION FUNCTIONS

;; NOTE: The generation process functions permanently change the property lists of the semantic network, so you'll need to reset the semantic network property lists before generating again in the same LISP image.  Suggest making the reset a function that you can call.

;; Functions corresponding to non-terminals.  Many of them need work.
;; The return value appears on the output, So if you only want to
;; modify a print image (which is the case with things like nbr) you
;; should do the modifications and return nil.

(defun nbr (structure)
  (setf (get structure 'ns) (append  (get structure 'ns) (list (get (get structure 'tok) 'pi)) ))
  nil
  )

(defun ns (structure)
  (get structure 'ns)
  )

(defun aux (structure)
  (setf
   (get structure 'vs)
   (append (list (get (get structure 'tok)
		      ;; grab the past or progressive form
		      ;; if required.
		      (cond ((equal (get structure 'aux) 'past) 'past)
			    ((equal (get structure 'aux) 'prog) 'prog)
			    (T 'pi)
			    )
		      )
		 )
	   (get structure 'vs)
	   )
   )
  nil
  )

(defun vs (structure)
  (get structure 'vs)
  )

;; before I had this so that it was putting the print image of the preposition
;; into the 'ns for the structure.  and this created a problem "A WITH BOTTLE"
;; when interacting with det and ns, the other terminal functions. Now the code just generates the
;; print image of the preposition as part of gen, which correctly creates "with a bottle"

(defun pobj (structure)
  (list (get (get (get structure 'pobj) 'tok) 'pi))
  )

(defun det (structure)
  (setf (get structure 'ns)
	(append
	 (if (equal (get structure 'det) 'indef) '(a) '(the))      
	 (get structure 'ns)
	 )
	)
  nil
  )

(defun generate-unary-rule (structure rule)
  (setf (get structure 'lab) (car rule))
  (format t "Match was a unary rule, so relabeling structure")
  (format t " ~a to ~a and calling gen on it recursively.~%" structure (car rule))
  (gen structure)
  )
  
(defun visit-terminal (structure rule)

  ;; If this new semantic structure node is a terminal node in the
  ;; grammar, the simmons paper recursively calls gen but first
  ;; applies the function corresponding to the structure label to the
  ;; structure if it's a terminal node, and I apply the function
  ;; corresponding to the structure label?  Do I really need to append
  ;; a call gen to that?  Yes, because ... it's possible to have more
  ;; than one terminal node in the grammar act on a semantic node.
  ;; For example, POBJ, NBR, DET, and NS are a sequence of terminal
  ;; nodes that will all need to act on the semantic node to produce a
  ;; string of the noun with the right determiner, singular/plural
  
  (format t "~a is a terminal node in the grammar so we're printing it but calling gen recursively on structure ~a.~%" (car rule) structure)
  (append (apply (car rule) (list structure)) (gen structure))
  )


(defun generate-binary-rule (structure rule)
  (format t "Generate based on matching between structure ~a and grammar path ~a.~%"
	       structure rule)
  ;; set k to the child semantic structure node that we're connecting to
  ;; The parent semantic structure node has an association corresponding to the grammar that points to
  ;; the appropriate child.  (e.g. if the grammar transition is "pobj np1" then our semantic node should have
  ;; a "pobj" association to another semantic node
  (setq k (get structure (car rule)))
  (format t "k was set to ~a. ~%" k)
  ;; relabel the semantic structure with the grammar node symbol at the end of the
  ;; transition
  (setf (get structure 'lab) (cadr rule))
  (format t "structure node ~a was relabeled to ~a.~%" structure (cadr rule)) 
 
  (cond
    ((get (car rule) 'terminal) (visit-terminal structure rule))

    ;; Not a terminal, so we're actually going to connect to a semantic structure node.
    ;; Label the new structure that we're visiting after the grammar non-terminal.
    ;; What I should probably do is decomp this code and build a couple of "visitor" functions
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


(defun gen (structure)
  (cond
    ;; don't really need this first line at the moment, since i'm outputting a print image below
    ((setq j (get structure 'terminal)) (print "terminal node. exiting.") (list j))

    ;; If this node in the semantic network isn't labeled, then exit.
    ;; but also set j to be the label of this node
    ;; do I really need this?  In what situation would I visit a structure node that isn't labeled?
    ((null (setq j (get structure 'lab))) (print "no structure label. exiting.") nil)
	
    ;; In this hacked version, won't check to see if label is in the grammar
    ;; if you get this far, you have a match between grammar and structure!
    ;; now i need to check all rules until I have a match
    ;; j is the grammar label on the semantic structure.  Get the paths from that
    ;; node in the grammar and see if any match the semantic structure
    ;; if there is no match, exit and return nil
    ((null (setq rule (get-matching-rule structure (get j 'paths))))  (format t "the structure ~a had nothing matching paths in the grammar state ~a.  exiting.~%" structure j) nil)

    ;; if there is a match now rule is set to the grammar rule
    ;; if it's a unary rule, just relabel the semantic structure to that node in the grammar
    ;; and call gen recursively
    ;; unary rules don't spawn new generations, just a recursive call to generate on the
    ;; same node.
    
    ((= 1 (length rule)) (generate-unary-rule structure rule))

    ;; if it's not a unary rule, then the grammar state symbol is the second element of rule
    
    (T (generate-binary-rule structure rule))
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

(defun reset-and-gen () 
  (reset-property-list)
  ;; Need to label the semantic network with the node in the grammar network where
  ;; you want generation to start.
  (setf (get 'c1 'lab) 's)
  ;; Evaluate this to generate starting at semantic network node c1.
  (print (gen 'c1))
  )

(reset-and-gen)

;; MORE NOTES

;; in the original paper, I think john is really an agent, not a
;; dative.  and why do they have a dative at the start of a sentence?
;; in the paper it says that dative is supposed to be a "deep case
;; relation". maybe this is meant for sentences like "it looks like"
;; or "it seems?"  but I noticed that in Table VI, the larger semantic
;; structure, they have john in the AGT relation with "see" instead of
;; as a dative.  so, the table III dative must be a bug.  going to
;; just use AGT for john, so below, notice for C1 it says (agt c2)
;; instead of (dat c2).

;; to test:
;; (get 'vp0 'paths)

;; Can view the property list for a symbol s with (symbol-plist 's)
;; Can clear the property list for a symbol with (setf (symbol-plist 's) nil)
;; can also set the property list using this too!

;; Some tests of functions
;; (apply 'nbr (list 'c3))
;; (get-matching-rule 'c3  '((dat pred) (agt pred) ) )
