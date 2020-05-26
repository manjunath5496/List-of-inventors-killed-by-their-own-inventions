;;
;; 6.090 IAP '05 - hw 7 solns
;;

; entry abstraction

(define (make-entry word synonyms)
  (list word synonyms))

(define (entry-word entry)
  (first entry))

(define (entry-synonyms entry)
  (second entry))

; thesaurus abstraction

(define (add-to-thesaurus entry thesaurus)
  (cons entry thesaurus))

(define (lookup word thesaurus)
  (if (null? thesaurus)
      #f
      (if (string=? word (entry-word (car thesaurus)))
	  (car thesaurus)
	  (lookup word (cdr thesaurus)))))

; sentence transform

(define (transmogrify word thesaurus)
  (let ((entry (lookup word thesaurus)))
    (if entry
	(pick-random (entry-synonyms entry))
	word)))

(define (transmogrify-sentence sentence thesaurus)
  (if (null? sentence)
      nil
      (cons (transmogrify (car sentence) thesaurus)
	    (transmogrify-sentence (cdr sentence) thesaurus))))
