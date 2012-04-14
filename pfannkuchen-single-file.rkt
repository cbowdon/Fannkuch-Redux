#lang racket/base

(require racket/list
         racket/contract
         racket/cmdline)

(provide (contract-out 
          [pfannkuchen (-> exact-positive-integer? pair?)]))


;; Three parts to this program - permutation algorithm, 
;; 'pancake flipping' fannkuch algorithm and main, which applies fannkuch
;; to each of the permutations and generates a checksum
;; C. Bowdon, 18th March 2012

;;;;;;;;;;;;;;;;;; PERMUTATIONS

;; Originally written in Clojure by Andy Fingerhut & Stuart Halloway

;; just holds a permutation: state, sign and count array
(struct perm (p s c))
(define (print-perm x) (printf "~a\t~a\t~a\n" (perm-p x) (perm-s x) (perm-c x)))

;; initializes n-1 permutations - this will be used for parallelization 
(define (init-permutations n)
  (let ([n-1 (sub1 n)])
    (define (ip-iter i p c all)      
      (if (= i n)
          all
          (let ([p2 (rotate-left-first-n p n)]
                [c2 (sub1-at-index c n-1)])
            (ip-iter (add1 i) p2 c2 (append all (list (perm p2 1 c2)))))))    
    (let ([p0 (build-list n (λ (x) (+ 1 x)))] 
          [c0 (build-list n (λ (x) (+ 1 x)))])             
      (ip-iter 1 p0 c0 (list (perm p0 1 c0))))))

;; (-> exact-nonnegative-integer? perm? perm?)
(define (next-permutation op)
  (if [negative? (perm-s op)]      
      (let ([n (length (perm-p op))] ; n could be passed in as an argument, for a minute speedup
            [spn (swap-at-indices (perm-p op) 1 2)])        
        (define (np-iter i p c)
          (cond [(= i n) (perm p 1 c)] ; return (new perm, old count)
                [(not (= (list-ref c i) 1)) (perm p 1 (sub1-at-index c i))] ; sub1 from count index j, return (new perm, new count)
                [(= i (sub1 n)) #f] ; no more permutations
                [else 
                 (let ([i+1 (add1 i)])                        
                   (np-iter i+1 (rotate-left-first-n p (add1 i+1)) (set-at-index c i i+1)))]))
        (np-iter 2 spn (perm-c op)))
      (perm (swap-at-indices (perm-p op) 0 1) -1 (perm-c op))))

;; Profiling note: these following auxiliary functions
;; make up about 20% of the run time in total. To be optimized,
;; but low priority.

;; return list that is identical to lst but with 1 subtracted from the nth element
;; (-> (listof number?) number? (listof number?))
(define (apply-to-index proc lst index)
  (append (take lst index) 
          (list (proc (list-ref lst index))) 
          (drop lst (add1 index))))

(define (sub1-at-index lst index)
  (apply-to-index sub1 lst index))

(define (add1-at-index lst index)
  (apply-to-index add1 lst index))

;; straightforward, get a copy of list with val at index set to new val
(define (set-at-index lst index val)
  (append (take lst index) 
          (list val) 
          (drop lst (add1 index))))

;; (-> (listof number?) number? (listof number?)) 
(define (rotate-left-first-n p n) 
  (append (cdr (take p n)) (list (car p)) (drop p n)))

;; returns array but with swapped values at indices
;; (-> list? exact-nonnegative-integer? exact-nonnegative-integer? list?)
(define (swap-at-indices p i j)
  (let* ([a (min i j)]
         [b (max i j)]
         [head (take p a)]
         [middle (cdr (drop (take p b) a))]
         [end (cdr (drop p b))])         
    (append head
            (list (list-ref p b))
            middle
            (list (list-ref p a))
            end)))


;;;;;;;;;;;;;;;;;; PANCAKE FLIPPING

;; Profiling note: 70% of time is spent here
;; How to optimize?

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
      (let ([lst (build-list n (λ (x) (add1 x)))])
        (define (pfiter p checksum maxflips)
          (if [not p]
              (begin                
                (printf "~a~nPfannkuchen(~a) = ~a~n" checksum n maxflips)
                (cons checksum maxflips))              
              (let ([flips (fannkuch (perm-p p))])
                (pfiter (next-permutation p)
                        ((if [> (perm-s p) 0] + -) checksum flips)
                        (if [> flips maxflips] flips maxflips)))))  
        (pfiter (perm lst 1 lst) 0 0))))

(pfannkuchen (command-line #:program "pfannkuchen"
                           #:args (n)
                           (string->number n)))

;; Test timings
;; 12 : 1304s
;; 11 : 100s 99.5 99.8
;; 10 : 8.1s
;; 9 : 0.7s

;; TODO
;; make the job dividing function:
;; for each of the n initial permutations
;; generate the next (n-1)! permutations
;; (combined, these should be the same as the whole list if gen'd from just 1 2 3...n.)
;; i.e. parallelize this