#lang racket

(require rackunit
         "clojure-translation.rkt")

;; With the particular order of generating permutations used in this
;; program, it turns out that each of the n consecutive "groups" of
;; (n-1)!  permutations begin with these permutations (example given
;; for n=6):

(define init-6-ans
  (list 
   (perm '(1 2 3 4 5 6) 1 '(1 2 3 4 5 6)) ; 1st
   (perm '(2 3 4 5 6 1) 1 '(1 2 3 4 5 5)) ; 121st
   (perm '(3 4 5 6 1 2) 1 '(1 2 3 4 5 4)) ; 241st
   (perm '(4 5 6 1 2 3) 1 '(1 2 3 4 5 3)) ; 361st
   (perm '(5 6 1 2 3 4) 1 '(1 2 3 4 5 2)) ; 481st 
   (perm '(6 1 2 3 4 5) 1 '(1 2 3 4 5 1)))) ; 601st


(define init-6 (init-permutations 6))
(define init-7 (init-permutations 7))

; get correct permutations from file
(define (parse-example-file in)
  (for/list ([l (in-lines in)])         
    (unless (eof-object? l)
      (map (compose (lambda (x) (- x 48)) char->integer) (string->list l)))))

(define permutations 
  (call-with-input-file "fannkuch-redux-permutations.txt"
    parse-example-file))

; check two perm structs are equal
(define (check-equal-perm? x y)
  (check-true
   (and (equal? (perm-p x) (perm-p y))
        (equal? (perm-c x) (perm-c y))
        (equal? (perm-s x) (perm-s y)))))

(define (test-perm n initial [limit 1e4])
  (define (p-iter n p results count)
    (if [or (false? p) (>= (add1 count) limit)]
        (cdr results)
        (p-iter n (next-permutation n p) (cons (next-permutation n p) results) (add1 count))))
  (reverse (p-iter n initial (cons initial '()) 0)))



(test-case 
 "Initial permutations correct"
 (check-equal? (length init-6) (length init-6-ans)) 
 (for-each (λ (x y) (check-equal-perm? x y)) init-6 init-6-ans) 
 (check-equal? (length (init-permutations 7)) 7))


(test-case
 "All permutations correct"
 ; length correct (and dependent on starting perm)
 (check-equal? (length (test-perm 7 (car init-7))) (length permutations))
 (check-equal? (length (test-perm 6 (second init-6))) 600)
 (check-equal? (length (test-perm 6 (third init-6))) 480)
 (check-equal? (length (test-perm 6 (fourth init-6))) 360)
 (check-equal? (length (test-perm 6 (fifth init-6))) 240)
 (check-equal? (length (test-perm 6 (sixth init-6))) 120)
 ; syncs up with the other initials
 (check-equal-perm? (list-ref (test-perm 6 (first init-6)) 120) (second init-6-ans))
 (check-equal-perm? (list-ref (test-perm 6 (first init-6)) 240) (third init-6-ans))
 (check-equal-perm? (list-ref (test-perm 6 (first init-6)) 360) (fourth init-6-ans))
 (check-equal-perm? (list-ref (test-perm 6 (first init-6)) 480) (fifth init-6-ans))
 (check-equal-perm? (list-ref (test-perm 6 (first init-6)) 600) (sixth init-6-ans))
 ; syncs up with the other initials (diff starting perm)
 (check-equal-perm? (list-ref (test-perm 6 (second init-6)) 0) (second init-6-ans))
 (check-equal-perm? (list-ref (test-perm 6 (second init-6)) 120) (third init-6-ans))
 (check-equal-perm? (list-ref (test-perm 6 (second init-6)) 240) (fourth init-6-ans))
 (check-equal-perm? (list-ref (test-perm 6 (second init-6)) 360) (fifth init-6-ans))
 (check-equal-perm? (list-ref (test-perm 6 (second init-6)) 480) (sixth init-6-ans))
 ; syncs up with the other initials (diff starting perm)
 (check-equal-perm? (list-ref (test-perm 6 (fifth init-6)) 120) (sixth init-6-ans))
 ; thorough check against the file
 (for-each
  (λ (x y) (check-equal? (perm-p x) y))
  (test-perm 7 (car init-7))
  permutations))
