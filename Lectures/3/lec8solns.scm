;;
;; 6.090 IAP '05 - animal guessing game
;;

(load-option 'format)

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

;; animal abstraction

(define (make-animal name)
  (list "animal" name))

(define (animal? x)
  (string=? (first x) "animal"))

(define (animal-name animal)
  (second animal))

(define (ask-about-animal animal)
  (ask-yes-no (string-append "Is it a " (animal-name animal))))

;; question tree abstraction

(define (make-question text yes-branch no-branch)
  (list "question" text yes-branch no-branch))

(define (question? x)
  (string=? (first x) "question"))

(define (question-text ques)
  (second ques))

(define (question-yes ques)
  (third ques))

(define (question-no ques)
  (fourth ques))

(define (ask-question ques)
  (ask-yes-no (question-text ques)))

;;

(define (list-guesser knowledge)
  (if (null? knowledge)
      (print-msg "I give up.")
      (if (ask-about-animal (car knowledge))
	  (print-msg "Yay!")
	  (list-guesser (cdr knowledge)))))

(define (new-list-guesser knowledge)
  (if (null? knowledge)
      (begin
	(print-msg "I give up.")
	(improve-list))
      (if (ask-about-animal (car knowledge))
	  (begin (print-msg "Yay!") knowledge)
	  (cons (car knowledge)
		(new-list-guesser (cdr knowledge))))))

(define (improve-list)
  (list (make-animal (read-in-string "What was your animal"))))

(define (tree-guesser knowledge)
  (if (animal? knowledge)
      (if (ask-about-animal knowledge)
	  (print-msg "Yay!")
	  (print-msg "I give up"))
      (if (ask-question knowledge)
	  (tree-guesser (question-yes knowledge))
	  (tree-guesser (question-no knowledge)))))

(define (new-tree-guesser knowledge)
  (if (animal? knowledge)
      (if (ask-about-animal knowledge)
	  (begin
	    (print-msg "Yay!")
	    knowledge)
	  (begin
	    (print-msg "I give up.")
	    (improve-tree knowledge)))
      (if (ask-question knowledge)
	  (make-question (question-text knowledge)
			 (new-tree-guesser (question-yes knowledge))
			 (question-no knowledge))
	  (make-question (question-text knowledge)
			 (question-yes knowledge)
			 (new-tree-guesser (question-no knowledge))))))

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

(define (play-game guesser knowledge)
  (let ((newqs (guesser knowledge)))
    (if (ask-yes-no "play again")
	(play-game guesser newqs)
	newqs)))

(define sample-list
  (list
   (make-animal "elephant")
   (make-animal "hummingbird")))

(define sample-tree
  (make-question
   "does it fly"
   (make-animal "hummingbird")
   (make-animal "elephant")))

(define larger-tree
  (make-question
   "does it fly"
   (make-question
    "is it an insect"
    (make-animal "mosquito")
    (make-animal "hummingbird"))
   (make-question
    "is it a mammal"
    (make-animal "elephant")
    (make-animal "iguana"))))
