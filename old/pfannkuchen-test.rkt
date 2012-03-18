#lang racket
(require rackunit
         profile
         "pfannkuchen.rkt")

;; for  7: checksum 228, max flips 16
;; for 12: 3968050, 65

(check-equal? (pfannkuchen 7) (cons 228 16))
;(check-equal? (pfannkuchen 12) (cons 3968050 65))

(define time-info
  (for/list ([i (in-range 6 9)])
    (call-with-values (λ () (time-apply pfannkuchen (list i)))
                      (λ (a b c d) (list a b c d)))))

;; notes on timing:
;; approximate relationship is (pfannkuchen n) takes 3*10^(n-6) ms
;; tested up to n = 10
(define (estimate-seconds n)
  (* 3 (expt 10 (- n 9))))

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

;(profile-thunk (λ () (pfannkuchen 10)))

;; The profiling suggests that most time (68%) is spent in flip, in fannkuch, in pfannkuchen.
;; I think that function is O(n^2/4) time, but don't know how it could be improved.
;; Making it mutable is likely to reduce performance, according to the Racket Guide.
;; I'm leaving that to one side for the moment.
;; The remaining time is spent in the permutations and particularly in a few helper functions:
;; swap-at-indices (4.5%)
;; apply-to-index (1.4%)
;; rotate-left-first-n (1.4%)
;; set-at-index (1.0%)

;;============================================================================
;;                                  Caller
;; Idx    Total         Self      Name+src                              Local%
;;        ms(pct)       ms(pct)     Callee
;;============================================================================
;;----------------------------------------------------------------------------
;;                                  pfiter [11]                         100.0%
;;[12] 24170(69.5%)  23863(68.6%) flip ...2-03-14-fannkuch-redux/pfannkuchen.rkt:9:2
;;----------------------------------------------------------------------------
;;                                  pfiter [11]                         100.0%
;;[13]  4580(13.2%)   3008(8.7%)  next-permutation ...x/clojure-translation.rkt:82:0
;;                                  swap-at-indices [15]                 34.3%
;;----------------------------------------------------------------------------
;;                                  pfiter [11]                         100.0%
;;[14]  2586(7.4%)    1260(3.6%)  np-iter ...kuch-redux/clojure-translation.rkt:86:8
;;                                  apply-to-index [16]                  18.9%
;;                                  rotate-left-first-n [17]             18.8%
;;                                  set-at-index [18]                    13.5%
;;----------------------------------------------------------------------------
;;                                  next-permutation [13]               100.0%
;;[15]  1571(4.5%)    1571(4.5%)  swap-at-indices ...ux/clojure-translation.rkt:67:0
;;----------------------------------------------------------------------------
;;                                  np-iter [14]                        100.0%
;;[16]   490(1.4%)     434(1.2%)  apply-to-index ...dux/clojure-translation.rkt:46:0
;;                                  loop [19]                            11.2%
;;----------------------------------------------------------------------------
;;                                  np-iter [14]                        100.0%
;;[17]   487(1.4%)     432(1.2%)  rotate-left-first-n ...lojure-translation.rkt:59:0
;;                                  loop [19]                            11.2%
;;----------------------------------------------------------------------------
;;                                  np-iter [14]                        100.0%
;;[18]   349(1.0%)     349(1.0%)  set-at-index ...redux/clojure-translation.rkt:53:0
;;----------------------------------------------------------------------------