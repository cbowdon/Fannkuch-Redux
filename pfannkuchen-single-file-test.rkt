#lang racket
(require rackunit
         profile
         "pfannkuchen-single-file.rkt")

(test-case
 "Correct final answer"
 ;; for  7: checksum 228, max flips 16
 (check-equal? (pfannkuchen 7) (cons 228 16))
 ;; for 12: 3968050, 65 (SLOW)
 ;(check-equal? (pfannkuchen 12) (cons 3968050 65))
 )

(define time-info
  (for/list ([i (in-range 6 9)])
    (call-with-values (λ () (time-apply pfannkuchen (list i)))
                      (λ (a b c d) (list a b c d)))))

;; notes on timing:
;; I found approximate relationship is (pfannkuchen n) takes 3*10^(n-6) ms
;; i.e. (pfannkuchen n) is O(10^n)
;; tested up to n = 10
(define (estimate-seconds n)
  (* 3 (expt 10 (- n 9))))

;; time-apply pfannkuchen (list 12) returned:
;; '((3968050 . 65))
;; 6,526,970
;; 6,906,280
;; 503923
;; i.e.correct answer, but 109 minutes to run
;; and about twice predicted time

(require plot)
(parameterize ([plot-y-transform log-transform])
  (plot (list
         (tick-grid)
         (points (map (λ (x y) (vector x (second y))) 
                      (build-list (length time-info) values) 
                      time-info)
                 #:color 'red
                 #:sym 'triangle)
         (points (map (λ (x y) (vector x (third y))) 
                      (build-list (length time-info) values) 
                      time-info)
                 #:color 'blue
                 #:sym 'square))))

;; SLOW
; (profile-thunk (λ () (pfannkuchen 10)))