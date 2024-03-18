#lang racket

;; zadanie 16
(define (append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))

(append '(1 2 3) 4)

(define (last l)
  (if (empty? (cdr l))
    (car l)
    (last (cdr l))))

(last '(1 2 3))

(define (reverse lis)
  (define (helper l acc)
    (if (empty? (cdr l))
      (append (list (car l)) acc)
      (helper (cdr l) (append (list (car l)) acc))))
  (helper lis '()))

(reverse '(1 2 3 4))

(define (delete x l)
  (define (helper l acc)
    (if (empty? (cdr l))
      (append acc (list (car l)))
      (if (= x (car l))
        (append acc (cdr l))
        (helper (cdr l) (append acc (list (car l)))))))
  (helper l '()))

(delete 3 '(1 2 3 4 3 5))

(define (pairing l1 l2)
  (define (helper h1 h2 acc)
    (if (empty? (cdr h1))
      (append acc (list (cons (car h1) (car h2))))
      (helper (cdr h1) (cdr h2) (append acc (list (cons (car h1) (car h2)))))))
  (when (= (length l1) (length l2))
    (helper l1 l2 '())))

(pairing '(1 2 3) '(a b c))

(define (split l x)
  (define (helper iter smaller bigger)
    (if (= (length iter) 0)
      (cons smaller (list bigger))
      (let ((head (car iter))
            (tail (cdr iter)))
        (if (< head x)
          (helper tail (append smaller (list head)) bigger)
          (helper tail smaller (append bigger (list head)))))))
  (helper l '() '()))

(split '(1 2 3 4) 3)

;; zadanie 17
(define (member2 x l)
  (define (traveler r)
    (if (= (length r) 0)
      #f
      (let ((head (car r))
            (tail (cdr r)))
        (if (list? head)
          (if (traveler head)
            #t
            (traveler tail))
          (if (= head x)
            #t
            (traveler tail))))))
  (traveler l))

(member2 3 (list 1 2 (list 3 4)))
(member2 5 (list 1 2 (list 3 4)))

;; zadanie 18
(define (square-list l)
  (define (square-up r acc)
    (if (= 0 (length r))
      acc
      (let ((head (car r))
            (tail (cdr r)))
        (square-up
          tail
          (append
            acc
            (list (* head head)))))))
  (square-up l '()))

(square-list '(1 2 3 4))

(define (mapf f l)
  (define (explorer iter acc)
    (if (= 0 (length iter))
      acc
      (let ((head (car iter))
            (tail (cdr iter)))
        (explorer tail (append acc (list (f head)))))))
  (explorer l '()))

(mapf (lambda (x) (* x x)) '(1 2 3 4))

(define (filter pred l)
  (define (traveler iter acc)
    (if (= 0 (length iter))
      acc
      (let ((head (car iter))
            (tail (cdr iter)))
        (traveler
          tail
          (if (pred head)
            (append acc (list head))
            acc)))))
  (traveler l '()))

(filter (lambda (x) (<= x 5)) '(1 2 3 4 5 5 5 6))

;; Używając list proszę stworzyć reprezentację
;; drzew binarnych i napisać funkcje:
;; (memberTree? element tree)
;; (sumTree tree)
;; (inorder tree)
;; (mapTree f tree)

;; zadanie 19
