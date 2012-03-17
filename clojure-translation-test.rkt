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

(define (check-equal-perm? x y)
  (and (equal? (perm-p x) (perm-p y))
       (equal? (perm-c x) (perm-c y))
       (equal? (perm-s x) (perm-s y))))

(test-case
 "init-permutations"
 (check-equal? (length init-6) (length init-6-ans)) 
 (for-each (λ (x y) (check-equal-perm? x y)) init-6 init-6-ans) 
 (check-equal? (length (init-permutations 7)) 7) )

; get correct permutations from file
(define (parse-example-file in)
  (for/list ([l (in-lines in)])         
    (unless (eof-object? l)
      (map (compose (lambda (x) (- x 48)) char->integer) (string->list l)))))

(define permutations 
  (call-with-input-file "fannkuch-redux-permutations.txt"
    parse-example-file))

(define init-7 (init-permutations 7))

(test-case
 "Next permutation"
 (check-equal? (perm-p (next-permutation 7 (car init-7))) (cadr permutations)))

(define (test-perm n initial limit)
  (define (p-iter n p results count)
    (if [or (false? p) (>= (add1 count) limit)]
        results
        (p-iter n (next-permutation n p) (cons (next-permutation n p) results) (add1 count))))
  (reverse (p-iter n initial (cons initial '()) 0)))

(length permutations)

(define n-to-print 30)
(for-each 
 (λ (x y) (printf "~a\t~a\t~a\n" (perm-p x) (perm-s x) y))
  (test-perm 7 (car init-7) n-to-print)
  (take permutations n-to-print))