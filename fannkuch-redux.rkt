#lang racket/base

(require racket/list)

(provide fannkuch)

; flips first (car list) elements of a list until (car list) is 1
; (-> (listof positive-integer?) (listof positive-integer?))
; I think this is O(n^2/4)
; dropping a list should be O(n/2) and reversing a sublist (average length n/2) should be O(n/2)
(define (fannkuch input)
  (define (flip lst number-of-flips)
    (if [= (car lst) 1]
        (values lst number-of-flips)
        (flip (flip-iter lst (drop lst (car lst)) (car lst)) (add1 number-of-flips))))  
  (define (flip-iter source result count)
      (if [= count 0]
          result
          (flip-iter (cdr source) (cons (car source) result) (sub1 count))))    
  (flip input 0))



