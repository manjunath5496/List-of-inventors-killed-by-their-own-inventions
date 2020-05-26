;;
;; nimrod-solns.scm - 6.090 IAP '05 project solutions (hw6)
;;


(load-option 'format)

(define (make-piles a b)
  (if (< a b)
      (list a b)
      (list b a)))

(define (pile-a piles)
  (first piles))

(define (pile-b piles)
  (second piles))

(define (random-piles min max)
  (make-piles (+ (random (- max min)) min)
	      (+ (random (- max min)) min)))

(define (empty-piles? piles)
  (and (= (pile-a piles) 0)
       (= (pile-b piles) 0)))

(define (valid-piles? piles)
  (and (>= (pile-a piles) 0)
       (>= (pile-b piles) 0)))

(define (display-piles piles)
  (format #t "~%Pile A: ~A     Pile B: ~A~%"
	  (pile-a piles) (pile-b piles)))

(define (make-move amta amtb)
  (list amta amtb))

(define (move-amta move)
  (first move))

(define (move-amtb move)
  (second move))

(define (valid-move? move)
  (and
   (or (= (move-amta move) (move-amtb move))
       (= (move-amta move) 0)
       (= (move-amtb move) 0))
   (>= (move-amta move) 0)
   (>= (move-amtb move) 0)
   (not
    (and (= (move-amta move) 0)
	 (= (move-amtb move) 0)))))

(define (apply-move piles move)
  (make-piles (- (pile-a piles) (move-amta move))
	      (- (pile-b piles) (move-amtb move))))


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


(define (safe-play-nimrod name1 strat1 name2 strat2 piles errors)
  (display-piles piles)
  (if (= errors 0)
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


(define (human-strat name piles)
  (format #t "~A how many to take from A: " name)
  (let ((a (read)))
    (format #t "~A how many to take from B: " name)
    (make-move a (read))))

(define (simple-strat name piles)
  (make-move 0 1))


(define (list-of-ints start end)
  (if (= start end)
      (list start)
      (cons start (list-of-ints (+ start 1) end))))

(define (without-n lst n)
  (if (null? lst)
      nil
      (if (= (car lst) n)
	  (cdr lst)
	  (cons (car lst)
		(without-n (cdr lst) n)))))

(without-n (list-of-ints 1 100) 7)

(define (make-lose-positions n)
  (lose-position-helper (list-of-ints 0 n) 0))

(define (lose-position-helper ints i)
  (if (null? ints)
      nil
      (cons (make-piles (car ints) (+ (car ints) i))
	    (lose-position-helper
	     (without-n (cdr ints) (+ (car ints) i))
	     (+ i 1)))))

(make-lose-positions 15)

(define lose-positions (make-lose-positions 100))

(define (move-to piles objective-piles)
  (let ((move (make-move (- (pile-a piles) (pile-a objective-piles))
			 (- (pile-b piles) (pile-b objective-piles)))))
    (if (valid-move? move)
	move
	(let ((move (make-move
		     (- (pile-a piles) (pile-b objective-piles))
		     (- (pile-b piles) (pile-a objective-piles)))))
	  (if (valid-move? move)
	      move
	      #f)))))
	  
(define (optimal-helper positions piles)
  (if (null? positions)
      (make-move 0 1)
      (let ((move (move-to piles (car positions))))
	(if move
	    move
	    (optimal-helper (cdr positions) piles)))))

(define optimal-strat
  (lambda (name piles)
    (optimal-helper lose-positions piles)))
