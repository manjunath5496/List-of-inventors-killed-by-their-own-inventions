;;
;; 6.090 IAP '05 lecture 3 - scheme code to load
;;

(load-option 'format) ; MITScheme printf equivalent

;;; tower of hanoi helper

; print disc moving text
(define move
  (lambda (disc from to)
    (format #t "move disc ~A from ~A to ~A~%" disc from to)))

;;; guess-a-number helper procedures

; returns a random number between low and high inclusive
(define (random-number low high)
  (+ low (random (+ (- high low) 1))))

; prompts for a number from the user
; user types in a number and presses C-x, C-e
(define (get-player-guess)
  (begin
    (format #t "~%Guess a number between 1 and 100:")
    (read)))

; displays a message to the user
(define (show-msg msg)
  (format #t "~%~A~%" msg))
