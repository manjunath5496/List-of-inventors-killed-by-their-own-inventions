;;
;; 6.090 IAP '05 - hw7code.scm
;;

(define (pick-random lst)
  (if (null? lst)
      nil
      (list-ref lst (random (length lst)))))

(define (string-explode s)
  (let ((space (string-search-forward " " s)))
    (if (not space)
	(list s)
	(cons (string-head s space)
	      (string-explode (string-tail s (+ space 1)))))))

(define (restore-string lst)
  (if (null? lst)
      ""
      (if (null? (cdr lst))
	  (car lst)
	  (string-append (car lst)
			 " "
			 (restore-string (cdr lst))))))

(define empty-thesaurus nil)

(define simple-thesaurus
  (add-to-thesaurus
   (make-entry "victory" (list "conquest" "triumph" "win"))
   (add-to-thesaurus
    (make-entry "ball" (list "globe" "orb" "rondure" "sphere"))
    (add-to-thesaurus
     (make-entry "store" (list "market" "outlet" "shop"))
     empty-thesaurus))))

(define full-thesaurus
  (add-to-thesaurus
   (make-entry "victory" (list "conquest" "triumph" "win"))
   (add-to-thesaurus
    (make-entry "ball" (list "globe" "orb" "rondure" "sphere"))
    (add-to-thesaurus
     (make-entry "store" (list "market" "outlet" "shop"))
     (add-to-thesaurus
      (make-entry "bat" (list "baton" "bludgeon" "cudgel" "truncheon"))
      (add-to-thesaurus
       (make-entry "hit" (list "strike" "clout" "whack"))
       (add-to-thesaurus
	(make-entry "edifice" (list "erection" "pile" "structure"))
	(add-to-thesaurus
	 (make-entry "museum" (list "gallery"))
	 (add-to-thesaurus
	  (make-entry "grand" (list "impressive" "gorgeous" "splendid"))
	  empty-thesaurus)))))))))

