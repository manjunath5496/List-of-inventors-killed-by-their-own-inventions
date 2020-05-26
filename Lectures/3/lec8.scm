;;
;; 6.090 IAP '05 - animal guessing game
;;

; hit M-o to load this file

(load-option 'format)

; prints out the question, prompting the user for a response
; returns #t if the answer is y and #f if it's n
(define (ask-yes-no question)
  (let ((c (prompt-for-command-char (string-append question 
						   " (y or n)?"))))
    (if (char-ci=? c #\y)
	#t
	(if (char-ci=? c #\n)
	    #f
	    (begin
	      (format #t "~%Answer y or n~%")
	      (ask-yes-no question))))))

; prints out the prompt, then reads in the string
; hit C-x C-e to submit the string
(define (read-in-string question)
  (format #t "~%~A~%" question)
  (format #t "(Please enter a string (surrounded by \"s) and use C-x, C-e to submit it)~%")
  (let ((val (read)))
    (if (string? val)
	val
	(read-in-string question))))

(define (print-msg text)
  (format #t "~%~A~%" text)
  #t)


;;;;;;;;;;;; animal abstraction

(define (make-animal name)
  to-be-completed)

(define (animal? x)
  to-be-completed)

(define (animal-name animal)
  to-be-completed)

;;;;;;;;;;;;

; returns #t if the player's animal is animal
(define (ask-about-animal animal)
  to-be-completed)

; takes a guesser procedure
; the guesser takes knowledge as input and returns
; an updated version of knowledge
(define (play-game guesser knowledge)
  (let ((newqs (guesser knowledge)))
    (if (ask-yes-no "play again")
	(play-game guesser newqs)
	newqs)))

;;;;;;;;;;;; list guesser

(define (list-guesser knowledge)
  to-be-completed)

; return a new improved knowledge upon success or failure
(define (new-list-guesser knowledge)
  to-be-completed)

; returns a list containing an animal read from the player
(define (improve-list)
  (list (make-animal (read-in-string "What was your animal"))))

;;;;;;;;;;;; question (node) abstraction

(define (make-question text yes-branch no-branch)
  to-be-completed)

(define (question? x)
  to-be-completed)

(define (question-text ques)
  to-be-completed)

(define (question-yes ques)
  to-be-completed)

(define (question-no ques)
  to-be-completed)

;;;;;;;;;;;;;

; asks the player the question text and returns #t or #f
(define (ask-question ques)
  to-be-completed)

;;;;;;;;;;;;;

(define (tree-guesser knowledge)
  to-be-completed)

(define (new-tree-guesser knowledge)
  to-be-completed)

(define (improve-tree animal)
  (let ((newa (read-in-string "What was your animal")))
    (let ((q (read-in-string (string-append "enter a question that differentiates a "
					    newa
					    " from a "
					    (animal-name animal)))))
      (let ((which (ask-yes-no (string-append "for a "
					      newa
					      " would the answer be yes or no"))))
	(if which
	    (make-question q
			   (make-animal newa)
			   animal)
	    (make-question q
			   animal
			   (make-animal newa)))))))


;(define sample-list
;  (list
;   (make-animal "elephant")
;   (make-animal "hummingbird")))

;(define sample-tree
;  (make-question
;   "does it fly"
;   (make-animal "hummingbird")
;   (make-animal "elephant")))

;(define larger-tree
;  (make-question
;   "does it fly"
;   (make-question
;    "is it an insect"
;    (make-animal "mosquito")
;    (make-animal "hummingbird"))
;   (make-question
;    "is it a mammal"
;    (make-animal "elephant")
;    (make-animal "iguana"))))
