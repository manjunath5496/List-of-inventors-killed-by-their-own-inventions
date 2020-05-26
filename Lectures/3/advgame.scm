;;
;; 6.090 IAP '05 - advgame.scm
;;

; game of MIT
;

;
; abstraction - student

;constructor - student name and alist of properties
(define (make-student name properties)
  (list name properties))

;selector - picks out name
(define (student-name student)
  (first student))

;selector - picks out alist of properties
(define (student-properties student)
  (second student))

;advanced selector - picks out value of particular property from alist
(define (student-value student property)
  (cadr (assoc property (student-properties student))))

;advanced constructor - builds a new student with property changed to newvalue
(define (student-update student property newvalue)
  (make-student (student-name student)
		(cons (list property newvalue)
		      (del-assoc property
				 (student-properties student)))))

;builds a pre-play student - initial values for all properties
(define (initial-student name)
  (make-student 
   name
   (list (list "sanity" 10)
	 (list "clue" 0)
	 (list "hand" nil)
	 (list "weapons" nil))))

;returns a new student with a 3 higher sanity score
(define (student-sleeps student)
  (format #t "~%~A sleeps deeply, regaining sanity.~%" (student-name student))
  (student-update student
		  "sanity"
		  (+ 3 (student-value student "sanity"))))

;displays status of student properties
(define (student-status student)
  (format #t "Student: ~A~%Sanity: ~A~%Clue: ~A~%"
	  (student-name student)
	  (student-value student "sanity")
	  (student-value student "clue"))
  (display-weapons (student-value student "weapons")))

;returns a new student with another card added to the hand property
(define (add-card-to-hand student card)
  (student-update student "hand"
		  (cons card (student-value student "hand"))))

;returns card i from the student's hand
(define (ith-card student i)
  (list-ref (student-value student "hand") i))

;returns a new student with card i removed from the student's hand
(define (without-ith-card student i)
  (let ((hand (student-value student "hand")))
    (student-update student
		    "hand"
		    (append (list-head hand i) (list-tail hand (+ i 1))))))

;displays the types and names of each card in the hand with their
; associated index
(define (display-cards i hand)
  (if (null? hand)
      "done"
      (begin
	(format #t "~A - ~A: ~A~%" i (caar hand) (cadar hand))
	(display-cards (+ i 1) (cdr hand)))))

;displays the hand and prompts user to pick a card
; returns the index of the card picked
(define (pick-card hand)
  (if (null? hand)
      (error "empty hand!")
      (begin
	(format #t "~%Your hand:~%")
	(display-cards 0 hand)
	(read-number "Pick a card" (- (length hand) 1)))))

;sums the utilities of the list of weapons
(define (weapons-effect weapons)
  (if (null? weapons)
      0
      (+ (weapon-utility (car weapons))
	 (weapons-effect (cdr weapons)))))

;computes the observed clue of the student, which
; is the sum of a tenth of their clue plus any weapon effects
; plus 2d4 (2 four-sided dice) for variability
(define (observed-cluefulness student)
  (let ((base-clue (quotient (student-value student "clue") 10))
	(weapons (weapons-effect (student-value student "weapons")))
	(randomness (+ 2 (random 4) (random 4))))
    (format #t "~%Base: ~A  Weapons: ~A Randomness: ~A ==> Total: ~A~%"
	    base-clue weapons randomness 
	    (+ base-clue weapons randomness))
    (+ base-clue weapons randomness)))

;weapon abstraction - entries in the weapon property of students

;constructor - name and effect on clue
(define (make-weapon name utility)
  (list name utility))

;selector for name
(define (weapon-name weap)
  (first weap))

;selector for effect on clue
(define (weapon-utility weap)
  (second weap))

;prints out name and effect on clue of list of weaps.
(define (display-weapons weaps)
  (if (null? weaps)
      "done"
      (begin
	(format #t "  ~A gives bonus of ~A~%" (weapon-name (car weaps))
		(weapon-utility (car weaps)))
	(display-weapons (cdr weaps)))))
     
;;;;; cards ;;;;;;


;;; pset card

;tag for pset cards
(define pset-tag "pset")

;constructor - name of pset and clue rating necessary to solve
(define (make-pset-card name difficulty)
  (list pset-tag name difficulty))

;selector for name
(define (pset-card-name card)
  (second card))

;selector for difficulty
(define (pset-card-difficulty card)
  (third card))

;procedure to perform effect of card - returns a new student
; after applying effect
(define (pset-card-proc card student)
  (format #t "~%~A tackles ~A..."
	  (student-name student)
	  (pset-card-name card))
  (if (>= (observed-cluefulness student) (pset-card-difficulty card))
      (begin
	(format #t "and breezes through it.  Another pset down!~%")
	(student-update student "clue" 
			(+ (pset-card-difficulty card)
			   (student-value student "clue"))))
      (begin
	(format #t "and crashes and burns!  Sanity lost!~%")
	(student-update student "sanity"
			(- (student-value student "sanity")
			   2)))))

;;; chance cards

(define chance-tag "chance")

;constructor for chance cards - card name and change to student sanity
(define (make-chance-card name sanitymod)
  to-be-completed)

;selector for card name
(define (chance-card-name card)
  to-be-completed)

;selector for card sanity mod
(define (chance-card-sanitymod card)
  to-be-completed)

;procedure to perform effect of card - returns a new student
; after applying effect
(define (chance-proc card student)
  (format #t "~%~A~%~A's sanity changes by ~A~%" (chance-card-name card)
	  (student-name student) (chance-card-sanitymod card))
  (student-update student "sanity"
		  INSERT-CODE-HERE))

;;; weapon cards

;weapon card tag
(define weapon-tag "weapon")

;constructor for weapon cards - name and effect of weapon on clue
(define (make-weapon-card name utility)
  (list weapon-tag name utility))

;selector for card (and thus) weapon name
(define (weapon-card-name wcard)
  (second wcard))

;selector for card (and thus) weapon utility
(define (weapon-card-utility wcard)
  (third wcard))

;procedure to perform effect of card - returns a new student
; after applying effect
(define (weapon-proc card student)
  (format #t "~%~A discovers the utility of ~A (~A)!"
	  (student-name student) (weapon-card-name card)
	  (weapon-card-utility card))
  (student-update student "weapons"
		  (cons (make-weapon (weapon-card-name card)
				     (weapon-card-utility card))
			(student-value student "weapons"))))

;alist of card tag -> card procedures
(define card-procs 
  (list
   (list pset-tag pset-card-proc)
   (list chance-tag chance-proc)
   (list weapon-tag weapon-proc)))

;look up procedure for card based on tag
(define (card-procedure card)
  (cadr (assoc (car card) card-procs)))

;run a turn for the student.  returns a new student value that
; reflects the change the turn had on the student.
(define (turn student)
  (format #t "~%Student ~A's turn:~%" (student-name student))
  (student-status student)
  (if (ask-yes-no "sleep instead of going to class")
      (student-sleeps student)
      (let ((play (pick-card (student-value student "hand"))))
	(let ((card (ith-card student play)))
	  ((card-procedure card)
	   card
	   (without-ith-card student play))))))

;has the student won?
(define (won? student)
  (>= (student-value student "clue") 30))

;has the student lost?
(define (lost? student)
  (<= (student-value student "sanity") 0))

;play the game
; deck is the deck of cards yet to be drawn.  when empty,
; it reshuffles the discard list into the deck list.
; stud1 is the student next to play.
(define (play-game deck discard stud1 stud2)
  (if (null? deck)
      (play-game (randomize-list discard) nil stud1 stud2)
      (if (< (length (student-value stud1 "hand")) 5)
	  (play-game (cdr deck) (cons (car deck) discard)
		     (add-card-to-hand stud1 (car deck)) stud2)
	  (let ((newstud (turn stud1)))
	    (if (won? newstud)
		(format #t "~A wins!~%" (student-name newstud))
		(if (lost? newstud)
		    (format #t "~A loses~%" (student-name newstud))
		    (play-game (cdr deck)
			       (cons (car deck) discard)
			       stud2
			       newstud)))))))
;shuffle a list
(define (randomize-list lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (if (= (random 2) 0)
	  (append (randomize-list (list-tail lst (quotient (length lst) 2)))
		  (randomize-list (list-head lst (quotient (length lst) 2))))
	  (append (randomize-list (list-head lst (quotient (length lst) 2)))
		  (randomize-list (list-tail lst (quotient (length lst) 2)))))))

;list of cards
(define cards
  (list
   (make-pset-card "trivial pset" 3)
   (make-pset-card "trivial pset" 3)
   (make-pset-card "easy pset" 5)
   (make-pset-card "easy pset" 5)
   (make-pset-card "easy pset" 5)
   (make-pset-card "easy pset" 5)
   (make-pset-card "easy pset" 5)
   (make-pset-card "easy pset" 5)
   (make-pset-card "hard pset" 7)
   (make-pset-card "hard pset" 7)
   (make-pset-card "hard pset" 7)
   (make-pset-card "impossible pset" 9)
   (make-pset-card "impossible pset" 9)
; UNCOMMENT THESE when you have implemented chance cards
;   (make-chance-card "It's a holiday!" 2)
;   (make-chance-card "Forget to eat lunch!" -1)
;   (make-chance-card "Forget to eat lunch!" -1)
;   (make-chance-card "Forget to eat lunch!" -1)
;   (make-chance-card "Eat some chocolate!" 1)
;   (make-chance-card "Eat some chocolate!" 1)
;   (make-chance-card "Eat some chocolate!" 1)
;   (make-chance-card "Get in a fight!" -2)
   (make-weapon-card "TA Office hours" 1)
   (make-weapon-card "Course bible" 1)
   (make-weapon-card "Lab assistants" 2)
   (make-weapon-card "Ben Bitdiddle" -1)
   (make-weapon-card "Caffeine" 3)))

; nifty format procedure
(load-option 'format)

;reads in a true/false value
(define (ask-yes-no question)
  (let ((c (prompt-for-command-char (string-append question 
						   " (y or n)? "))))
    (display c)
    (if (char-ci=? c #\y)
	#t
	(if (char-ci=? c #\n)
	    #f
	    (begin
	      (format #t "~%Answer y or n~%")
	      (ask-yes-no question))))))

;reads in a digit between 0 and max inclusive
(define (read-number text max)
  (let ((c (prompt-for-command-char (format #f "~A (0..~A)? " text max))))
    (display c)
    (if (and (char-numeric? c) (<= (char->digit c) max))	
	(char->digit c)
	(begin
	  (format #t "~%Answer with digit in (0..~A)~%" max)
	  (read-number text max)))))

;Example play-game:
;(play-game nil cards (initial-student "ben") (initial-student "jen"))

