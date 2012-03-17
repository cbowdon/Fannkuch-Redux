#lang racket/base

(require racket/list)

;; Written in Clojure by Andy Fingerhut & Stuart Halloway,
;; translated here to Racket by Chris Bowdon on 2012/03/17
;; I've made it immutable, so this version is probably using
;; more memory - but it's clearer (to me at least)
;; Note that also my next-permutation sets the sign itself, 
;; it doesn't leave that to the caller

(provide perm
         perm-p
         perm-s
         perm-c
         print-perm
         init-permutations
         next-permutation)

; just holds a permutation: state, sign and count array
(struct perm (p s c))
(define (print-perm x) (printf "~a\t~a\t~a\n" (perm-p x) (perm-s x) (perm-c x)))

; initializes n-1 permutations as above
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


; return list that is identical to lst but with 1 subtracted from the nth element
; (-> (listof number?) number? (listof number?))
(define (sub1-at-index lst index)
  (apply-to-index sub1 lst index))

(define (add1-at-index lst index)
  (apply-to-index add1 lst index))

(define (apply-to-index proc lst index)
  ; TODO: this could be optimized easily
  (append (take lst index) 
          (list (proc (list-ref lst index))) 
          (drop lst (add1 index))))

(define (set-at-index lst index val)
  ; TODO: could this be optimized?
  (append (take lst index) 
          (list val) 
          (drop lst (add1 index))))

; (-> (listof number?) number? (listof number?)) 
(define (rotate-left-first-n p n) 
  ; TODO: this could be optimized
  (append (cdr (take p n)) (list (car p)) (drop p n)))


; returns array but with swapped values at indices
; (-> list? exact-nonnegative-integer? exact-nonnegative-integer? list?)
(define (swap-at-indices p i j)
  ; TODO: i think this could be optimized
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

; (-> exact-nonnegative-integer? perm? perm?)
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

; TODO
; make the job dividing function:
; for each of the n initial permutations
; generate the next (n-1)! permutations
; (combined, these should be the same as the whole list if gen'd from just 1 2 3...n.)