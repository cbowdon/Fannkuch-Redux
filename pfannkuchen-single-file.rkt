#lang racket/base

(require racket/list
         racket/vector
         racket/contract
         racket/cmdline
         racket/future)

(provide (contract-out 
          [pfannkuchen (-> exact-positive-integer? pair?)]))

;; Three parts to this program - permutation algorithm, 
;; 'pancake flipping' fannkuch algorithm and main, which applies fannkuch
;; to each of the permutations and generates a checksum
;; C. Bowdon, 18th March 2012

;;;;;;;;;;;;;;;;;; PERMUTATIONS

;; This part originally written in Clojure by Andy Fingerhut & Stuart Halloway

;; just holds a permutation: state, sign and count array
(struct perm (p s c))

;; initializes n-1 permutations - used for parallelization
;; isn't actually used in the serial implementation
(define (init-permutations! n)
  (let ([n-1 (sub1 n)])
    (define (ip-iter i p c all)      
      (if (= i n)
          (reverse all)
          (let ([p2 (rotate-left-first-n! (vector-copy p) n)]
                [c2 (sub1-at-index! (vector-copy c) n-1)])
            (ip-iter (add1 i) p2 c2 (cons (perm p2 1 c2) all)))))    
    (let ([p0 (build-vector n (λ (x) (+ 1 x)))] 
          [c0 (build-vector n (λ (x) (+ 1 x)))])             
      (ip-iter 1 p0 c0 (cons (perm p0 1 c0) '())))))

;; generate the next permutation (has side effects!)
;; (-> exact-nonnegative-integer? perm? perm?)
(define (next-permutation! op)  
  (if [negative? (perm-s op)]      
      (let ([n (vector-length (perm-p op))])        
        (define (np-iter! i p c)          
          (cond [(= i n) (perm p 1 c)] ; return (new perm, old count)
                [(not (= (vector-ref c i) 1)) (perm p 1 (sub1-at-index! c i))] ; sub1 from count index j, return (new perm, new count)
                [(= i (sub1 n)) #f] ; no more permutations
                [else 
                 (let ([i+1 (add1 i)])                   
                   (np-iter! i+1 (rotate-left-first-n! p (add1 i+1)) (set-at-index! c i i+1)))]))
        (np-iter! 2 (swap-at-indices! (perm-p op) 1 2) (perm-c op)))      
      (perm (swap-at-indices! (perm-p op) 0 1) -1 (perm-c op))))

;; subtract 1 from the nth element
;; (-> vector? number? vector?)
(define (apply-to-index! proc v index)
  (begin 
    (vector-set! v index (proc (vector-ref v index)))
    v))

(define (sub1-at-index! c i)
  (apply-to-index! sub1 c i))

;; set val at index to new val
(define (set-at-index! v i j)
  (begin (vector-set! v i j) v))

;; (-> vector? number? vector?) 
(define (rotate-left-first-n! v n)
  (let ([temp (vector-ref v 0)])
    (vector-copy! v 0 v 1 n)
    (vector-set! v (sub1 n) temp)
    v))

;; returns array but with swapped values at indices
;; (-> vector? exact-nonnegative-integer? exact-nonnegative-integer? vector?)
(define (swap-at-indices! v i j)
  (let ([temp (vector-ref v i)])
    (vector-set! v i (vector-ref v j))
    (vector-set! v j temp)
    v))

;; factorial
;; (-> exact-nonnegative-integer?  exact-nonnegative-integer?)
(define (factorial x)
  (define (f-iter y result)
    (cond [(= 0 y) result]
          [else (f-iter (sub1 y) (* y result))]))
  (f-iter x 1))

;;;;;;;;;;;;;;;;;; PANCAKE FLIPPING

;; perform flipping on a single permutation, return number of flips
(define (fannkuch input)
  ;; perform a single rearrangement
  (define (single-flip source result count)
    (if [= count 0]
        result
        (single-flip (cdr source) (cons (car source) result) (sub1 count))))
  ;; flip until (car lst) is 1
  (define (multi-flip lst number-of-flips)
    (let ([l (car lst)])
      (if [= l 1]
          number-of-flips
          (multi-flip (single-flip lst (drop lst l) l) (add1 number-of-flips)))))  
  (multi-flip input 0))

;;;;;;;;;;;;;;;;;; MAIN

(define (pfannkuchen n)
  (if [< n 3] 
      #f
      (let ([orig-p (build-vector n (λ (x) (add1 x)))]
            [orig-c (build-vector n (λ (x) (add1 x)))])
        (define (pf-iter p checksum maxflips)          
          (if [not p]
              (begin                
                (printf "~a~nPfannkuchen(~a) = ~a~n" checksum n maxflips)
                (cons checksum maxflips))              
              (let ([flips (fannkuch (vector->list (perm-p p)))])
                (pf-iter (next-permutation! p)
                         ((if [> (perm-s p) 0] + -) checksum flips)
                         (if [> flips maxflips] flips maxflips)))))  
        (pf-iter (perm orig-p 1 orig-c) 0 0))))

;; parallel version 

;; parallel version auxiliaries
(define (limited-pfannkuchen op limit)
  (define (pf-iter p checksum maxflips count)          
    (if [or (not p) (>= count limit)]
        (cons checksum maxflips)              
        (let ([flips (fannkuch (vector->list (perm-p p)))])
          (pf-iter (next-permutation! p)
                   ((if [> (perm-s p) 0] + -) checksum flips)
                   (if [> flips maxflips] flips maxflips)
                   (add1 count)))))  
  (pf-iter op 0 0 0))

;; testing - it works
(define (parts n)
  (map
   (λ (x) (limited-pfannkuchen x (factorial (sub1 n))))
   (init-permutations! n)))

(define z (command-line #:program "pfannkuchen"
                        #:args (n)
                        (string->number n)))

(define ans (pfannkuchen z))
(require rackunit)
(check-equal? (foldl (λ (x y) (+ (car x) y)) 0 (parts z)) (car ans))
(check-equal? (apply max (map cdr (parts z))) (cdr ans))

(define (tasks n)
  (map (λ (x) (future (λ () (limited-pfannkuchen x (factorial (sub1 n))))))
       (init-permutations! n)))

;; NOT parallel actually...
(define para (map touch (tasks z)))

(check-equal? (foldl (λ (x y) (+ (car x) y)) 0 para) (car ans))
(check-equal? (apply max (map cdr para)) (cdr ans))

(require lib/time)
(time-repeat (pfannkuchen z) #:repeat 1)
;(time-repeat (map touch (tasks z)) #:repeat 1)

;; test-times Intel Corei5, OS X Lion (MBP 2011)
;; shows that mutable permutation version is slightly faster
;; n : mut, immut
;; 12: 901s, 1038s
;; 11: 65s, 81s, 
;; 10 : 4.8s, 6.3s
;; 9 : 0.4s, 0.6s

;; TODO
;; Parallelize this
;; i.e. Make the job dividing function:
;; For each of the n initial permutations generate the next (n-1)! permutations
;; (combined, these should be the same as the whole list if gen'd from just 1 2 3...n).
;; Checksum = sum of checksums of n separate (n-1)! permutations
;; Max flips = max of max flips of n separate (n-1)! permutations
;; 
;; Pseudocode
;; (parallel-map pfannkuchen-of-next-(n-1)!-permutations 
;;               (init permutations n)) 
;; --> (listof (cons checksum maxflips))
;; ---> answers are (foldl + 0 n-checksums) (max n-maxflips)
;; where pfannkuchen-of-next-(n-1)!-permutations is just pfannkuchen above,
;; but limited to (n-1)! permutations
;; Presumably some kind of concurrent mutable data structure will be required
;; for the final step of adding the checksums and maxflips

