#lang racket

;; definitions

;; data structure
(struct perm (p s c))
(define (perm->string x) (format "~a\t~a\t~a" (perm-p x) (perm-s x) (perm-c x)))

;; original
;; slightly improved
(define (init-permutations n)
  (let ([n-1 (sub1 n)])
    (define (ip-iter i p c all)      
      (if (= i n)
          (reverse all)
          (let ([p2 (rotate-left-first-n p n)]
                [c2 (sub1-at-index c n-1)])
            (ip-iter (add1 i) p2 c2 (cons (perm p2 1 c2) all)))))    
    (let ([p0 (build-list n (λ (x) (+ 1 x)))] 
          [c0 (build-list n (λ (x) (+ 1 x)))])             
      (ip-iter 1 p0 c0 (cons (perm p0 1 c0) '())))))

;; aux for original
(define (rotate-left-first-n p n) 
  (append (cdr (take p n)) (list (car p)) (drop p n)))

(define (apply-to-index proc lst index)
  (append (take lst index) 
          (list (proc (list-ref lst index))) 
          (drop lst (add1 index))))

(define (sub1-at-index c i)
  (apply-to-index sub1 c i))

;; mutable (faster, 1.5x)
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

;; aux for mutable
(define (apply-to-index! proc v index)
  (begin 
    (vector-set! v index (proc (vector-ref v index)))
    v))

(define (sub1-at-index! c i)
  (apply-to-index! sub1 c i))

(define (rotate-left-first-n! v n)
  (let ([temp (vector-ref v 0)])
    (vector-copy! v 0 v 1 n)
    (vector-set! v (sub1 n) temp)
    v))

(define (make-copy-perm orig) 
  (perm (vector-copy (perm-p orig))
        (perm-s orig)
        (vector-copy (perm-c orig))))

;; testing
(require rackunit
         lib/time)

(define (pv->pl pv)
  (perm (vector->list (perm-p pv)) 
        (perm-s pv) 
        (vector->list (perm-c pv))))

(define (pl->pv pl)
  (perm (list->vector (perm-p pl)) 
        (perm-s pl) 
        (list->vector (perm-c pl))))

(define (check-equal-perm? x y)
  (check-true
   (and (equal? (perm-p x) (perm-p y))
        (equal? (perm-c x) (perm-c y))
        (equal? (perm-s x) (perm-s y)))))


(define init-6-ans
  (list 
   (perm '(1 2 3 4 5 6) 1 '(1 2 3 4 5 6)) ; 1st
   (perm '(2 3 4 5 6 1) 1 '(1 2 3 4 5 5)) ; 121st
   (perm '(3 4 5 6 1 2) 1 '(1 2 3 4 5 4)) ; 241st
   (perm '(4 5 6 1 2 3) 1 '(1 2 3 4 5 3)) ; 361st
   (perm '(5 6 1 2 3 4) 1 '(1 2 3 4 5 2)) ; 481st 
   (perm '(6 1 2 3 4 5) 1 '(1 2 3 4 5 1)))) ; 601st

(define init-6 (init-permutations 6))
(define init-7 (init-permutations 7))

(define init!-6 (init-permutations! 6))
(define init!-7 (init-permutations! 7))

(time-repeat (init-permutations 6) #:repeat 1e6)
(time-repeat (init-permutations! 6) #:repeat 1e6)

;(for-each
; (λ (x y) (printf "~a~n~a~n~n" (perm->string x) (perm->string y)))
; init-6
; init!-6)


(for-each
 (λ (x y) (check-equal-perm? x (pv->pl y)))
 init-6
 init!-6)

(for-each
 (λ (x y) (check-equal-perm? x (pv->pl y)))
 init-7
 init!-7)