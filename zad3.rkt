#lang racket
;; 10) Proszę napisać funkcję (product term a next b) analogicznie do funkcji sum z wykładu.
;;       Proszę pokazać, jak używając product można definiować funkcję silnia oraz przybliżać pi na podstawie formuły
;;       pi/4 = 2 · 4 · 4 · 6 · 6 · 8 ··· / 3 · 3 · 5 · 5 · 7 · 7 ··· .
;;
;; Ad 10) Funkcja sum z wykładu:
;; (define (sum term next a b) (
;;                                 if(> a b) 0
;;                                 (+ (term a) (sum term next (next a) b)))
;; )
;; Przykład użycia:
;; (define (sum-integers a b)(sum identity add1 a b))
;; (sum-integers 1 4)

(define (sum term next a b)
  (if (> a b)
    0
    (+ (term a)
       (sum term next (next a) b))))

(define (sum-integers a b)
  (sum (lambda (x) (+ x 1)) add1 a b))

(sum-integers 1 4)

(define (mult term next a b)
  (if (> a b)
    1
    (* (term a)
       (mult term next (next a) b))))

(define (mult-integers a b)
  (mult
    (lambda (x)
      (if (= x b)
          x
          (* x x)))
    (lambda (x) (+ x 2))
    a
    b))

(mult-integers 4 8)

(define (get-pi lim)
  (* 4
     (/
       (* 2
          (mult-integers
            4.0
            (+ lim 2)))
       (mult-integers
         3.0
         (+ lim 2)))))

(get-pi 6)

;; 11) Proszę napisać funkcję (accumulate combiner null-value term a next b),
;; która jest uogólnieniem funkcji sum i prod. Argumenty term, next,
;; a i b zachowują to same znaczenie niż w funkcjach sum i prod. combiner
;; jest dwuargumentową funkcją, która opisuje, jak (term a) zostaje
;; dodane do akkumulacji dalszych termów. null-value jest wartością
;; inicjalną do używania w końcowym przypadku.
;; Proszę pokazać, jak można definiować funkcje sum i prod używając accumulate. 

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(define (sum-acc term next a b)
  (accumulate + 0 term a next b))

(sum-acc identity add1 1 4)

(define (mult-acc term next a b)
  (accumulate * 1 term a next b))

(mult-acc identity add1 1 4)

;; 12) Nawet funkcję accumulate można dalej uogólnić. Proszę
;; napisać funkcję filter-accumulate, która ma dodatkowy argument pred.
;; pred jest (jednoargumentowym) predykatem a filter-accumulate dodaje
;; (term a) do wynika tylko, jeżeli a spełnie predykat pred.
;; Używając filter-accumulate proszę obliczyć sumę kwadratów
;; liczb pierwszych w interwale [a,b] oraz produkt wszystkich liczb
;; naturalnych i mniejsze niż n, takie że nwd(i,n) = 1.

(define (filter-accumulate pred combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (if (pred a)
                (term a)
                null-value)
              (filter-accumulate pred combiner null-value term (next a) next b))))

(require math/number-theory)
(define (square-sum a b)
  (filter-accumulate prime? + 0 (lambda (x) (* x x)) a add1 b))

(square-sum 2 3)
