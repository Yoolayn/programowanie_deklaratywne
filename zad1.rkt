#lang racket

;;; zadanie 1
10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ ( * 2 4) (- 4 6)) ;; 6

(let
  ((a 1)
   (b 2))
  (+ a b (* a b))) ;; 5

a ;; undefined

(define a 3)
a ;; 3

(define b (+ a 1)) ;; 4
(+ a b (* a b)) ;; 19
(= a b) ;; #f

(if
  (and
    (> b a)
    (< b (* a b)))
  b
  a) ;; b => 4

(cond
  ((= a 4) 6)
  ((= b 4) (+ 6 7 a))
  (else 25)) ;; (+ 6 7 a) => 16

;;; zadanie 2
(define new.< <)

(define new.>
  (lambda (a b)
    (not (new.< a b))))

(define new.=
  (lambda (a b)
    (and
      (not (new.< a b))
      (not (new.< b a)))))

(define new.<=
  (lambda (a b)
    (or
      (and
        (not (new.< a b))
        (not (new.< b a)))
      (new.< a b))))

(define new.>=
  (lambda (a b)
    (or
      (and
        (not (new.< a b))
        (not (new.< b a)))
      (not (new.< a b)))))

(define new.<>
  (lambda (a b)
    (not (and
           (not (new.< a b))
           (not (new.< b a))))))

;; zadani 3
(define nwd
  (lambda (x y)
    (if (= y 0)
      x
      (nwd y (modulo x y)))))

(define nww
  (lambda (x y)
    (if (= b 0)
      a
      (/ (* x y) (nwd x y)))))

(nww 2 3)
(nww 5 10)

(modulo 5 2) ;; reminder
(quotient 5 2) ;; whole division

;;; zadanie 4
(define (odd? n)
  (if (zero? n)
    #f
    (even? (sub1 n))))

(define (even? n)
  (if (zero? n)
    #t
    (odd? (sub1 n))))

;;; zadanie 5
(define same-values?
  (lambda (p1 p2 x y)
    (equal? (p1 x y) (p2 x y))))

(same-values? = new.= 4 4)

(if (and
      (> b a) ;; b > a
      (< b
         (* a b))) ;; b < (a*b)
  ;; if true:
  "true"
  ;; else:
  "false")

;; create a list
(define li '(1 2 3 4 5 6))

(define (sum li)
  (define (helper lit acc)
    (if (empty? lit)
      acc
      (helper (cdr lit) (+ acc (car lit)))))
  (helper li 0))
(sum li)

(define hello
  (lambda ()
    (letrec
      ((me "Magda")
       (I "Madeline")
       (myself "Madie")
       (Hi (list me myself I)))
      Hi)))

(hello)

(let ((x 10))
  (cond
    ((> x 10) 10)
    (else 20)))
