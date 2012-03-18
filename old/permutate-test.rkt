#lang racket

(require rackunit
         "permutate.rkt")

(check-equal? (length (permutate 1)) 1)
(check-equal? (length (permutate 2)) 2)
(check-equal? (length (permutate 3)) 6)
(check-equal? (length (permutate 4)) 24)
(check-equal? (length (permutate 5)) 120)
(check-equal? (length (permutate 6)) 720)
(check-equal? (length (permutate 7)) 5040)

(for ([i (in-range 2 8)])
  (let ([p (permutate i)])
    (check-equal? 
     (length p) 
     (length (remove-duplicates p)))))