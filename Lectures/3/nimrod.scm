;;
;; nimrod.scm - 6.090 IAP '05 project (hw6)
;;

(load-option 'format)

;; PILES abstraction

; constructor
(define (make-piles a b)
  to-be-completed)

;selector: returns smaller of two piles
(define (pile-a piles)
  to-be-completed)

;selector: returns larger of two piles
(define (pile-b piles)
  to-be-completed)

;;;;; abstraction barrier ;;;;;

; returns piles w/ counts between min and max, randomly
(define (random-piles min max)
  (make-piles (+ (random (- max min)) min)
	      (+ (random (- max min)) min)))

;returns true if piles is empty (no rods left in either pile)
(define (empty-piles? piles)
  to-be-completed)

;returns true if the pile counts are "valid"
(define (valid-piles? piles)
  to-be-completed)

;prints out piles in a human-friendly manner
(define (display-piles piles)
  (format #t "~%Pile A: ~A     Pile B: ~A~%"
	  (pile-a piles) (pile-b piles)))

;; MOVE abstraction

;constructor
(define (make-move amta amtb)
  (list amta amtb))

;returns amount to remove from pile a
(define (move-amta move)
  (first move))

;returns amount to remove from pile b
(define (move-amtb move)
  (second move))

;;;;; abstraction barrier ;;;;;

;returns true if move is valid (adheres to valid move specification)
(define (valid-move? move)
  to-be-completed)

;returns a new piles after the effect of move on original piles
(define (apply-move piles move)
  to-be-completed)

;main loop procedure to play a game of nimrod
; input is the names and strategies of the two players
;    along with the state of the piles
; a STRATEGY is a PROCEDURE that when given a name and piles,
;   returns a move.
; An example:
;  (play-nimrod "Ben" human-strat "Jen" human-strat (random-piles 5 15))
(define (play-nimrod name1 strat1 name2 strat2 piles)
  (display-piles piles)
  (if (empty-piles? piles)
      (format #t "~A wins! ~A loses!~%" name2 name1)
      (let ((move (strat1 name1 piles)))
	(format #t "~A takes ~A from pile A and ~A from pile B~%"
		name1 (move-amta move) (move-amtb move))
	(if (and (valid-move? move)
		 (valid-piles? (apply-move piles move)))
	    (play-nimrod name2 strat2 name1 strat1 
			 (apply-move piles move))
	    (begin
	      (format #t "Bad move ~A, try again:~%" name1)
	      (play-nimrod name1 strat1 name2 strat2 piles))))))

;"safe" version that tolerates a certain number of errors before halting
(define (safe-play-nimrod name1 strat1 name2 strat2 piles errors)
  (display-piles piles)
  (if (< errors 0)
      (format #t "Error tolerance exceeded; terminating!~%")
      (if (empty-piles? piles)
	  (format #t "~A wins! ~A loses!~%" name2 name1)
	  (let ((move (strat1 name1 piles)))
	    (format #t "~A takes ~A from pile A and ~A from pile B~%"
		    name1 (move-amta move) (move-amtb move))
	    (if (and (valid-move? move)
		     (valid-piles? (apply-move piles move)))
		(safe-play-nimrod name2 strat2 name1 strat1 
				  (apply-move piles move) errors)
		(begin
		  (format #t "Bad move ~A, try again:~%" name1)
		  (safe-play-nimrod name1 strat1 name2 strat2
				    piles (- errors 1))))))))


;a STRATEGY that prompts the user for their desired move.
(define (human-strat name piles)
  (format #t "~A: how many to take from A: " name)
  (let ((a (read)))
    (format #t "~A: how many to take from B: " name)
    (make-move a (read))))

;a stupid STRATEGY that takes one from the greater pile, always
(define (simple-strat name piles)
  (make-move 0 1))

;;; writing the optimal strategy

;returns a list of numbers from start to end, inclusive
(define (list-of-ints start end)
  to-be-completed)

;returns a new list like lst, except without the number n
(define (without-n lst n)
  to-be-completed)

;returns a list of lose positions (a list of piles) in order
; starting with (0 0)
(define (make-lose-positions n)
  (lose-position-helper (list-of-ints 0 n) 0))

;given a list of unused integers and the index i, generates
; the list of lose positions starting with the ith lose position
(define (lose-position-helper ints i)
  (if (null? ints)
      nil
      (cons (make-piles **your-code-here**)
	    (lose-position-helper
	     **your-code-also-here**
	     (+ i 1)))))

; when you've implemented make-lose-positions, uncomment and
; evaluate the following:
;(define lose-positions (make-lose-positions 100))

;given the current piles and the objective piles, return either
; a valid move to make the transition or #f if one does not exist.
; This is sticky!  You will want to use let.
(define (move-to piles objective-piles)
  to-be-completed)
	  
;a STRATEGY that plays optimally!
(define optimal-strat
  (lambda (name piles)
    (optimal-helper lose-positions piles)))

;searchs through the lose positions for one that can
; be moved to in 1 move.  returns the move or something dumb
(define (optimal-helper positions piles)
  (if (null? positions)  ;; no more positions?
      (make-move 0 1)    ;; try something stupid
      (let ((move (move-to piles (car positions))))
	(if move         ;; could we move to first position?
	    move         ;; if so, do so, check check rest
	    (optimal-helper (cdr positions) piles)))))
