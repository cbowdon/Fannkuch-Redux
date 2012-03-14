#lang racket/base

(require rackunit
         "fannkuch-redux.rkt")

; first items from shoutout.alioth.debian.org
(define input (list '(1 2 3 4 5 6 7)
                    '(2 1 3 4 5 6 7)
                    '(2 3 1 4 5 6 7)
                    '(3 2 1 4 5 6 7)
                    '(3 1 2 4 5 6 7)
                    '(1 3 2 4 5 6 7)
                    '(2 3 4 1 5 6 7)
                    '(3 2 4 1 5 6 7)))

; answers worked out manually
(define output (list '(1 2 3 4 5 6 7)
                     '(1 2 3 4 5 6 7)
                     '(1 2 3 4 5 6 7)
                     '(1 2 3 4 5 6 7)
                     '(1 2 3 4 5 6 7)
                     '(1 3 2 4 5 6 7)
                     '(1 3 2 4 5 6 7)
                     '(1 3 2 4 5 6 7)))

; get permutations from file
(define (parse-example-file in)
  (for/list ([l (in-lines in)])         
    (unless (eof-object? l)
      (map (compose (lambda (x) (- x 48)) char->integer) (string->list l)))))

(define permutations 
  (call-with-input-file "fannkuch-redux-permutations.txt"
    parse-example-file))


(require profile)

(profile-thunk (λ () (for-each fannkuch permutations)))

(test-case
 "flipping works"
 (check-equal? (map (λ (x) (car (fannkuch x))) input) output))

(test-case
 "checksum works"
 (check-equal? (fannkuch-checksum permutations) 228))






