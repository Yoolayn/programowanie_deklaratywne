#lang racket

;; 20
(define (root f a b)
  (let ((TOL 0.0001)
        (NMAX 1000))
    (define (sign x)
      (cond
        ((> x 0) 1)
        ((= x 0) 0)
        (else -1)))
    (define (helper iter x y)
      (let ((c (/ (+ x y) 2))
            (chk (/ (- y x) 2)))
        (if (or (= (f c) 0) (< chk TOL) )
          (exact->inexact c)
          (if (= (sign (f c)) (sign (f x)) )
            (helper (add1 iter) c y)
            (helper (add1 iter) x c)))))
    (helper '1 a b)))


(root (lambda (x) (- (expt x 3) x 2)) -2 2)

;; 22
(define (prod l)
  (foldl * 1 l))

(prod '(1 2 3 4))

(define (length l)
  (foldl
    (lambda (x s)
      (add1 s))
    0
    l))

(length '(1 2 3 4))

(define (delete x l)
  (foldr
    (lambda (c s)
      (if (= c x)
        s
        (cons c s)))
    '()
    l))

(delete 5 '(1 2 3 4 5 5 6))

(define (reverse l)
  (foldl
    (lambda (c s)
      (cons c s))
    '()
    l))

(reverse '(1 2 3 4))

(define (map f l)
  (foldr
    (lambda (c s)
      (cons (f c) s))
    '()
    l))

(map (lambda (x) (* x x)) '(1 2 3 4))

(define (filter pred l)
  (foldr
    (lambda (c s)
      (if (pred c)
        (cons c s)
        s))
    '()
    l))

(filter (lambda (x) (= x 5)) '(1 2 3 4 5 5 6))

(define (forall pred l)
  (foldr
    (lambda (c s)
      (if (not s)
        s
        (if (pred c)
          #t
          #f)))
    '()
    l))

(forall (lambda (x) (= x 2)) '(2 2 2 2))

;; 23
(define (plus x y . l)
  (define (add a b)
    (if (= b 0)
      a
      (if (< b 0)
        (add (sub1 a) (add1 b))
        (add (add1 a) (sub1 b)))))
  (foldr add (add x y) l))

(plus 1 2 3 4 5 6)

(define (par . l)
  (lambda (x)
    (foldr
      (lambda (c s)
        (cons (c x) s))
      '()
      l)))

((par (lambda (x) x) (lambda (x) (* x x)) (lambda (x) (* x x x))) 3)
((par (lambda (x) (* x 2)) (lambda (x) (* x 2)))5)
