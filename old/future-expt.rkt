#lang racket/base

(require racket/future
         racket/fixnum
         racket/list
         lib/time) 

(define (any-double? l)
  (for/or ([i (in-list l)])
    (for/or ([i2 (in-list l)])
      (= i2 (* 2 i)))))

(define l1 (for/list ([i (in-range 30000)])
             (+ (* 2 i) 1)))
(define l2 (for/list ([i (in-range 30000)])
             (- (* 2 i) 1)))
(define l3 (for/list ([i (in-range 30000)])
             (- (* 2 i) 1)))
(define l4 (for/list ([i (in-range 30000)])
             (- (* 2 i) 1)))
(define l5 (for/list ([i (in-range 30000)])
             (- (* 2 i) 1)))
(define l6 (for/list ([i (in-range 30000)])
             (- (* 2 i) 1)))
(define l7 (for/list ([i (in-range 30000)])
             (- (* 2 i) 1)))

(define lx (build-list 4 (λ (j) (for/list ([i (in-range (* j 1e4))]) (- (* 2 i) 1)))))

;(time (or (any-double? l1)
;         (any-double? l2)
;        (any-double? l3)
;       (any-double? l4)
;      (any-double? l5)
;     (any-double? l6)
;    (any-double? l7)))

;(time (let ([f2 (future (lambda () (any-double? l2)))]
;            [f3 (future (lambda () (any-double? l3)))]
;            [f4 (future (lambda () (any-double? l4)))]
;            [f5 (future (lambda () (any-double? l4)))]
;            [f6 (future (lambda () (any-double? l4)))]
;            [f7 (future (lambda () (any-double? l4)))])
;        (or (any-double? l1)
;            (touch f2)
;            (touch f3)
;            (touch f4)
;            (touch f5)
;            (touch f6)
;            (touch f7))))
;
;(time (let ([f (map (λ (x) (future (λ () (any-double? x)))) (list l1 l2 l3 l4 l5 l6 l7))])
;       (map touch f)))

;(time (ormap any-double? lx))

;(time (let ([f (map (λ (x) (future (λ () (any-double? x)))) lx)]) (ormap touch f)))

(define (test1 n)
  (for ([i (in-range n)]) 
    (fx= i 2567)))

(define (test2 n)
  (for ([i (in-range 1 (fx+ 1 n))]) 
    (fx= i 2356)))

(define (test3 n)
  (for ([i (in-range 1 (fx+ 2 n))]) 
    (fx= i 9876)))

(define (test4 n)
  (for ([i (in-range 1 (fx+ 3 n))]) 
    (= i 1234)))

(define a 10000000)
(time-repeat
 (let (;[f1 (future (λ () (test1 a)))]
       [f2 (future (λ () (test2 a)))]
       [f3 (future (λ () (test3 a)))]
       [f4 (future (λ () (test4 a)))])
   (and (test1 a) 
        (touch f2)
        (touch f3)
        (touch f4))))
