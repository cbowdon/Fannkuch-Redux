#lang racket/base

(require racket/vector
         racket/list
         lib/time)

;; Here are efforts to optimize the innermost loop of fannkuchen using vectors.
;;
;; The innermost loop reverses the first n elements of a list/vector (this is called a 'flip').
;; My conclusions after this are that I can't optimize the innermost loop with vectors.

;; 1. Original list version
;; unflipped-list, tail-elements-not-to-flip, n -> flipped-list
;; e.g.
;; (single-flip '(3 4 5 6 7) '(6 7) 3) -> '(5 4 3 6 7) 
;; see more usage below
(define (single-flip source result count)
  (if [= count 0]
      result
      (single-flip (cdr source) (cons (car source) result) (sub1 count))))

;; 2. Naive vector version
;; unflipped-vector, n -> same-vector (now flipped)
(define (reverse-first-n source n)
  (begin 
    (vector-copy! source 
                  0 
                  (list->vector (reverse (vector->list (vector-take source n))))
                  0
                  n)
    source))

;; 3. Mutating elements one-by-one vector version
;; unflipped-vector, listof-first-n-elements, counter -> same-vector (now flipped)
(define (one-by-one source first-n n count)
  (cond [(null? first-n) source]
        [else 
         (begin
           (vector-set! source (- n count 1) (car first-n))    
           (one-by-one source (cdr first-n) n (add1 count)))]))


(define n 1)
(define v (vector 12 11 10 9 8 7 6 5 4 3 2 1)) ; is mutated. Shouldn't make time difference though (?)
(define s (list 12 11 10 9 8 7 6 5 4 3 2 1))

;(time-repeat (single-flip s (drop s n) n) #:repeat 1e7)
;(time-repeat (reverse-first-n v n) #:repeat 1e7)
;(time-repeat (one-by-one v (vector->list (vector-take v n)) n 0) #:repeat 1e7)

(define sf-times (list 503 689 876 1077 1458 1662 1906 2231 2398 2761 2898 3267))
(define rfn-times (list 4299 4981 5557 6003 6953 7419 8150 8914 9407 10342 11329 11712))
(define obo-times (list 2418 2840 3276 3790 4321 4820 5429 6115 6630 7067 7566 8268))
(define errors-ms (build-list 12 (λ (x) 200))) ; generously assuming up to 200 ms error
(define ns (build-list 12 values))

(require plot)
(parameterize ([plot-x-label "n"]
               [plot-y-label "Time for 10m runs (ms)"])
  (plot (list (tick-grid)
              (points (map vector ns sf-times) #:label "List" #:sym 'circle #:color 'blue)
              (points (map vector ns rfn-times) #:label "Naive vector" #:sym 'triangle #:color 'red)
              (points (map vector ns obo-times) #:label "Iterative vector" #:sym 'diamond #:color 'darkgreen)
              (error-bars (map vector ns sf-times errors-ms) #:color 'blue)
              (error-bars (map vector ns rfn-times errors-ms) #:color' red)
              (error-bars (map vector ns obo-times errors-ms) #:color 'darkgreen))))