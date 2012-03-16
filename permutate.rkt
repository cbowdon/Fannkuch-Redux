#lang racket

(provide permutate)

; create tree of permutations
(define (permutation-tree set [result '()])
  (if (> (length set) 1)
      (map (lambda (x) (permutation-tree (remove x set) (cons x result) )) set)
      (cons (car set) result)))
  
; walk tree and flatten to depth 1
(define (flat-1 t [results '()])  
  (cond 
    ; when empty return
    [(null? t) results]
    ; when lists ahead, process each half and append
    [(list? (caar t)) (append (flat-1 (car t) results) (flat-1 (cdr t) results))]
    ; when numbers ahead, add numbers to results and move on
    [(number? (caar t)) (flat-1 (cdr t) (cons (car t) results))] 
    ; other cond = error
    [else (error "flat-1 error" t)]))
         
         
; generate list of permutations for numbers in set length n (1-based)
(define (permutate n)
  (if [> n 1]
      (flat-1 
       (permutation-tree 
        (build-list n (lambda (x) (+ 1 x)))))
      (list n)))

