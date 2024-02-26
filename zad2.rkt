#lang racket

;; just recurency
(define sil-r
  (lambda (x)
    (if (= x 1)
      1
      (* x (sil-r (sub1 x))))))

(sil-r 4)

;; recurency without a tail
(define (sil-ak x)
  (define (helper x ak)
    (if (= x 1)
      ak
      (helper (sub1 x) (* ak x))))
  (helper x 1))

(sil-ak 4)

(define (fib-r x)
  (cond
    ((= x 1) 1)
    ((= x 0) 0)
    (else
      (+ (fib-r (- x 1)) (fib-r (- x 2))))))

(fib-r 4)
