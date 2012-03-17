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

;;============================================================================
;;                                  Caller
;; Idx    Total         Self      Name+src                              Local%
;;        ms(pct)       ms(pct)     Callee
;;============================================================================
;;----------------------------------------------------------------------------
;;                                  profile-thunk12 [9]                 100.0%
;;[10] 34776(100.0%)  1038(3.0%)  run ...acket v5.2.1/collects/profile/main.rkt:29:2
;;                                  pfiter [11]                          97.0%
;;----------------------------------------------------------------------------
;;                                  run [10]                            100.0%
;;[11] 33737(97.0%)   2401(6.9%)  pfiter ...3-14-fannkuch-redux/pfannkuchen.rkt:24:8
;;                                  flip [12]                            71.6%
;;                                  next-permutation [13]                13.6%
;;                                  np-iter [14]                          7.7%
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
;;                                  rotate-left-first-n [17]             49.8%
;;                                  apply-to-index [16]                  50.2%
;;[19]   110(0.3%)     110(0.3%)  loop ...cket v5.2.1/collects/racket/list.rkt:106:2
;;----------------------------------------------------------------------------
;;> 