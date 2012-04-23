#lang racket/base

(require racket/list
         racket/vector
         racket/bool         
         racket/future
         racket/fixnum
         lib/time)

(provide next-permutation!
         pl->pv)

;; Testing mutable vector versions of this auxiliary functions in the permutation side
;; Testing for speed and accuracy vs. their counterparts

;; original
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

;; mutable (faster, ~10x)
(define (swap-at-indices! v i j)
  (let ([temp (vector-ref v i)])
    (vector-set! v i (vector-ref v j))
    (vector-set! v j temp)
    v))

(define l (list 1 2 3 4 5 6 7 8))
(define v (list->vector l))

;(time-repeat (swap-at-indices l 4 5) #:repeat 1e7)
;(time-repeat (swap-at-indices! v 4 5) #:repeat 1e7)

;; originals
(define (apply-to-index proc lst index)
  (append (take lst index) 
          (list (proc (list-ref lst index))) 
          (drop lst (add1 index))))

(define (set-at-index lst index val)
  (append (take lst index) 
          (list val) 
          (drop lst (add1 index))))

(define (sub1-at-index c i)
  (apply-to-index sub1 c i))

;; mutable (faster, ~10x)
(define (apply-to-index! proc v index)
  (begin 
    (vector-set! v index (proc (vector-ref v index)))
    v))

(define (sub1-at-index! c i)
  (apply-to-index! (λ (x) (fx- x 1)) c i))

(define (set-at-index! v i j)
  (begin (vector-set! v i j) v))

;(time-repeat (apply-to-index sub1 l 4) #:repeat 1e7)
;(time-repeat (apply-to-index! sub1 v 4) #:repeat 1e7)

;(time-repeat (set-at-index l 4 6) #:repeat 1e7)
;(time-repeat (vector-set! v 4 6) #:repeat 1e7)


;; original
(define (rotate-left-first-n p n) 
  (append (cdr (take p n)) (list (car p)) (drop p n)))

;; mutable (faster, ~5x)
(define (rotate-left-first-n! v n)
  (let ([temp (vector-ref v 0)])
    (vector-copy! v 0 v 1 n)
    (vector-set! v (sub1 n) temp)
    v))

;(time-repeat (rotate-left-first-n l 6) #:repeat 1e7)
;(time-repeat (rotate-left-first-n! v 6) #:repeat 1e7)

(struct perm (p s c))
(define (perm->string x) (format "~a\t~a\t~a" (perm-p x) (perm-s x) (perm-c x)))

;; original
(define (next-permutation op)
  (if [negative? (perm-s op)]      
      (let ([n (length (perm-p op))])        
        (define (np-iter i p c)
          (cond [(fx= i n) (perm p 1 c)] ; return (new perm, old count)
                [(not (fx= (list-ref c i) 1)) (perm p 1 (sub1-at-index c i))] ; sub1 from count index j, return (new perm, new count)
                [(fx= i (fx- n 1)) #f] ; no more permutations
                [else 
                 (let ([i+1 (fx+ 1 i)])                        
                   (np-iter i+1 (rotate-left-first-n p (fx+ 1 i+1)) (set-at-index c i i+1)))]))
        (np-iter 2 (swap-at-indices (perm-p op) 1 2) (perm-c op)))
      (perm (swap-at-indices (perm-p op) 0 1) -1 (perm-c op))))

;; if positive, swap, change sign and continue
;; if negative, 

;; mutable (faster, ~3x)

(define (make-copy-perm orig) (perm (build-vector 7 (λ (x) (vector-ref (perm-p orig) x)))
                                    (perm-s orig)
                                    (build-vector 7 (λ (x) (vector-ref (perm-c orig) x)))))

(define (next-permutation! op)  
  (if [negative? (perm-s op)]      
      (let ([n (vector-length (perm-p op))])        
        (define (np-iter! i p c)          
          (cond [(fx= i n) (perm p 1 c)] ; return (new perm, old count)
                [(not (fx= (vector-ref c i) 1)) (perm p 1 (sub1-at-index! c i))] ; sub1 from count index j, return (new perm, new count)
                [(fx= i (fx- n 1)) #f] ; no more permutations
                [else 
                 (let ([i+1 (fx+ 1 i)])                   
                   (np-iter! i+1 (rotate-left-first-n! p (fx+ 1 i+1)) (set-at-index! c i i+1)))]))
        (np-iter! 2 (swap-at-indices! (perm-p op) 1 2) (perm-c op)))      
      (perm (swap-at-indices! (perm-p op) 0 1) -1 (perm-c op))))

(define opl (perm '(1 2 3 4 5 6) 1 '(1 2 3 4 5 6)))
(define opv (perm (vector 1 2 3 4 5 6) 1 (vector 1 2 3 4 5 6)))

;(time-repeat (next-permutation opl) #:repeat 1e7)
;(time-repeat (next-permutation! opv) #:repeat 1e7)

;; original
(define (init-permutations n)
  (let ([n-1 (fx- n 1)])
    (define (ip-iter i p c all)      
      (if (fx= i n)
          (reverse all)
          (let ([p2 (rotate-left-first-n p n)]
                [c2 (sub1-at-index c n-1)])
            (ip-iter (fx+ 1 i) p2 c2 (cons (perm p2 1 c2) all)))))    
    (let ([p0 (build-list n (λ (x) (fx+ 1 x)))] 
          [c0 (build-list n (λ (x) (fx+ 1 x)))])             
      (ip-iter 1 p0 c0 (cons (perm p0 1 c0) '())))))

;; mutable (faster, 1.5x)
(define (init-permutations! n)
  (let ([n-1 (fx- n 1)])
    (define (ip-iter i p c all)      
      (if (fx= i n)
          (reverse all)
          (let ([p2 (rotate-left-first-n! (vector-copy p) n)]
                [c2 (sub1-at-index! (vector-copy c) n-1)])
            (ip-iter (fx+ 1 i) p2 c2 (cons (perm p2 1 c2) all)))))    
    (let ([p0 (build-vector n (λ (x) (fx+ 1 x)))] 
          [c0 (build-vector n (λ (x) (fx+ 1 x)))])             
      (ip-iter 1 p0 c0 (cons (perm p0 1 c0) '())))))

(define (factorial x)
  (define (f-iter y result)
    (cond [(fx= 0 y) result]
          [else (f-iter (fx- y 1) (fx* y result))]))
  (f-iter x 1))


;;;;; testing ;;;;;

(require rackunit)

; get correct permutations from file
(define permutations
  (let ([parse-example-file 
         (λ (in)
           (for/list ([l (in-lines in)])         
             (unless (eof-object? l)
               (map (compose (lambda (x) (fx- x 48)) char->integer) (string->list l)))))])
    (call-with-input-file "fannkuch-redux-permutations.txt"
      parse-example-file)))

; check two perm structs are equal
(define (check-equal-perm? x y)
  (check-true
   (and (equal? (perm-p x) (perm-p y))
        (equal? (perm-c x) (perm-c y))
        (equal? (perm-s x) (perm-s y)))))

(define (pv->pl pv)
  (perm (vector->list (perm-p pv)) 
        (perm-s pv) 
        (vector->list (perm-c pv))))

(define (pl->pv pl)
  (perm (list->vector (perm-p pl)) 
        (perm-s pl) 
        (list->vector (perm-c pl))))

;; its a lot faster
;; but i hate side-effects

(define (test-perm initial [limit 10000])
  (define (p-iter p results count)
    (if [or (false? p) (fx>= count limit)]
        (cdr results)
        (p-iter (next-permutation p) (cons (next-permutation p) results) (fx+ 1 count))))
  (reverse (p-iter initial (cons initial '()) 0)))

(for-each
 (λ (x y) (check-equal? (perm-p x) y))
 (test-perm (perm '(1 2 3 4 5 6 7) 1 '(1 2 3 4 5 6 7)))
 permutations)

(define (test-perm! initial [limit 10000])
  (define (make-copy-perm orig) 
    (perm (vector-copy (perm-p orig))
          (perm-s orig)
          (vector-copy (perm-c orig))))
  (let ([results (make-vector limit)])
    (define (p-iter p count)
      ;(for-each (λ (x) (if [or (false? x) (number? x)] (printf "~a~n" x) (printf "~a~n" (perm->string x)))) (vector->list results))
      (if [or (false? p) (fx>= count limit)]                     
          results
          (begin
            (let* ([cp (make-copy-perm p)]
                   [np (next-permutation! cp)])
              (vector-set! results count p)              
              ;(printf "~a\t~a~n" count (perm->string p))
              (p-iter np (fx+ 1 count))))))
    (p-iter initial 0)))

;(time-repeat (test-perm! (perm (vector 1 2 3 4 5 6 7) 1 (vector 1 2 3 4 5 6 7))) #:repeat 1e2)
;(time-repeat (test-perm (perm '(1 2 3 4 5 6 7) 1 '(1 2 3 4 5 6 7))) #:repeat 1e2)

(for ([i (test-perm (perm '(1 2 3 4 5 6 7) 1 '(1 2 3 4 5 6 7)))]
      [j (test-perm! (perm (vector 1 2 3 4 5 6 7) 1 (vector 1 2 3 4 5 6 7)))]
      [k (in-range 5040)])
  (check-equal? (perm-p j) (list->vector (perm-p i)))
  (check-equal? (perm-s j) (perm-s i))
  (check-equal? (perm-c j) (list->vector (perm-c i))))

;; testing parallelization

;; original
(define (get-segments num)
  (for/list ([i (init-permutations num)])                        
    (test-perm i (factorial (fx- num 1)))))  
(define joined (apply append (get-segments 7)))                    

(for-each
 (λ (x y) (check-equal? (perm-p x) y))
 joined
 permutations)

;; mutatable
(define (get-segments! num)
  (for/list ([i (init-permutations! num)])                        
    (vector->list (test-perm! i (factorial (fx- num 1))))))
(define joined! (apply append (get-segments! 7)))                    

(for-each
 (λ (x y) (check-equal? (vector->list (perm-p x)) y))
 joined!
 permutations)

;; SAME LIST OF PERMUTATIONS IS GENERATED! Hallelujah

;; quick parallelism test
;; it blocks on a few vector functions
;; perhaps let's just use places

(define zx 10)

(define ip (init-permutations zx))
(define ip! (init-permutations! zx))

(time-repeat
 (for/list ([i ip!])                        
   (test-perm! i (factorial (sub1 zx)))))

(time-repeat
 (let ([f (for/list ([i ip!]) 
            (future (λ () (test-perm! i (factorial (sub1 zx))))))])
   (map touch f)))
;
;(time-repeat
; (let ([f (for/list ([i ip!]) 
;            (future (λ () (test-perm! i (factorial (sub1 zx))))))])
;   (map touch f)))
;
;(time-repeat
; (let ([f1 (future (λ () (test-perm (first ip) (factorial (sub1 zx)))))]
;       [f2 (future (λ () (test-perm (second ip) (factorial (sub1 zx)))))]
;       [f3 (future (λ () (test-perm (third ip) (factorial (sub1 zx)))))]
;       [f4 (future (λ () (test-perm (fourth ip) (factorial (sub1 zx)))))]
;       [f5 (future (λ () (test-perm (fifth ip) (factorial (sub1 zx)))))]
;       [f6 (future (λ () (test-perm (sixth ip) (factorial (sub1 zx)))))]
;       [f7 (future (λ () (test-perm (seventh ip) (factorial (sub1 zx)))))]
;       [f8 (future (λ () (test-perm (eighth ip) (factorial (sub1 zx)))))]
;       [f9 (future (λ () (test-perm (ninth ip) (factorial (sub1 zx)))))]
;       [f10 (future (λ () (test-perm (tenth ip) (factorial (sub1 zx)))))])
;   (map touch (list f1 f2 f3 f4 f5 f6 f7 f8 f9 f10))))




;; none are running in parallel
;; why? futures ---> places?
;; or just use racket/fixnum

