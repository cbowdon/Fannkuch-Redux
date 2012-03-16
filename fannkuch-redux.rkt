#lang racket/base

(require racket/list)

(provide fannkuch
         fannkuch-checksum)

; flips first (car list) elements of a list until (car list) is 1
; returns flipped list and number of flips it required
; (-> (listof positive-integer?) (cons (listof positive-integer?) positive-integer?))
; I think this is O(n^2/4)
; dropping a list should be O(n/2) and reversing a sublist (average length n/2) should be O(n/2)
(define (fannkuch input)
  (define (flip lst number-of-flips)
    (if [= (car lst) 1]
        (cons lst number-of-flips)
        (flip (flip-iter lst (drop lst (car lst)) (car lst)) (add1 number-of-flips))))  
  (define (flip-iter source result count)
    (if [= count 0]
        result
        (flip-iter (cdr source) (cons (car source) result) (sub1 count))))    
  (flip input 0))

; checksum-keeping version of fannkuch for mapping
; (-> (listof (listof positive-integer?)) positive-integer?)
; checksum = checksum + (if permutation_index is even then flips_count else -flips_count)
(define (fannkuch-checksum listofinput)
  (define (fc-iter loi sum index max-nflips)
    (if [null? loi]
        (cons sum max-nflips)
        (let ([count (cdr (fannkuch (car loi)))])
          (fc-iter (cdr loi) 
                   ((if [even? index] + -) sum count) 
                   (add1 index)
                   (if [> count max-nflips] count max-nflips)))))          
  (fc-iter listofinput 0 0 0))

