#lang racket/base

(require racket/list
         "clojure-translation.rkt")

(provide pfannkuchen)

(define (fannkuch input)
  ; flip-iter is aux to flip - it performs a single rearrangement
  (define (flip-iter source result count)
    (if [= count 0]
        result
        (flip-iter (cdr source) (cons (car source) result) (sub1 count))))      
  ; flip is aux to fannkuch - it flip-iters until (car lst) is 1
  (define (flip lst number-of-flips)
    (let ([l (car lst)])
      (if [= l 1]
          number-of-flips
          (flip (flip-iter lst (drop lst l) l) (add1 number-of-flips)))))  
  (flip input 0))

(define (pfannkuchen n)
  (if [< n 3] 
      #f
      (let ([lst (build-list n (λ (x) (add1 x)))])
        (define (pfiter p checksum maxflips)
          (if [not p]
              (cons checksum maxflips)
              (let ([flips (fannkuch (perm-p p))])
                (pfiter (next-permutation p)
                        ((if [> (perm-s p) 0] + -) checksum flips)
                        (if [> flips maxflips] flips maxflips)))))  
        (pfiter (perm lst 1 lst) 0 0))))