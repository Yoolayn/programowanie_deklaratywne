;; 6) Proszę pokazać, jak wyrazienie (sil 3) zostaje ewaluowane - raz dla wersji rekurencyjnej a raz dla wersji z akumulatorem.
;; 7) Proszę napisać rekurencyjną oraz z akumulatorem funkcję (fib n), która obliczy n-tą liczbę Fibonacci.
;; Proszę pokazać, jak wyrazienie (fib 3) zostaje ewaluowane - raz dla wersji rekurencyjnej a raz dla wersji z akumulatorem.
;; 8) Potęgowanie b^e (dla liczbe naturalnej e) można szybko wykonać używając b^e = (b^(e/2))^2. Proszę napisać rekurencyjną oraz z akumulatorem funkcję (exp b e) na podstawie tej właściwości.
;; Proszę pokazać, jak wyrażenie (exp 2 6) zostaje ewaluowane - raz dla wersji rekurencyjnej a raz dla wersji z akumulatorem.
;; 9) Dlaczego wyrażenie if nie można sam zdefiniować w następujący sposób?
;; (define (new-if warunek alternatywa1 alternatywa2)
;;    (cond (warunek alternatywa1)
;;          (else    alternatywa2)))

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

(define (fib-ak x)
  (define (helper n a b)
    (cond
      ((= n 0) a)
      ((= n 1) b)
      (else (helper (- n 1) b (+ a b)))))
  (helper x 0 1))

(fib-ak 10)

(define (exp-r b e)
  (define (square x)
    (* x x))
  (cond
    ((= e 0) 1)
    ((even? e) (square (exp-r b (/ e 2))))
    (else (* b (exp-r b (- e 1))))))

(exp-r 2 5)

(define (exp-ak b e)
  (define (helper b e ak)
    (cond
      ((= e 0) ak)
      ((even? e) (helper (* b b) (/ e 2) ak))
      (else (helper b (- e 1) (* ak b)))))
  (helper b e 1))

(exp-ak 2 5)

;; 9) Dlaczego wyrażenie if nie można sam zdefiniować w następujący sposób?
;; (define (new-if warunek alternatywa1 alternatywa2)
;;    (cond (warunek alternatywa1)
;;          (else    alternatywa2)))

;; ponieważ condition musi być zdefiniowane przed użyciem przez cond

(define (new-if condition alt1 alt2)
  (cond
    (condition alt1)
    (else alt2))
