#lang racket

(define (fak-iter n result)
  (if (> n 0) (fak-iter (- n 1) (* result n)) result))



(define (euler-iter n result)
  (if (> n 0) (euler-iter (- n 1)
                          (+ result (/ 1 (fak-iter n 1)))) result))


(define (euler-n n) (euler-iter n 1))



(define (max-iter n m) (let ((c_max (remainder n 10)))
  (if (> (quotient n 10) 0) (max-iter (quotient n 10) (if (> c_max m) c_max m)) m)))

 

(define (maxziffer n)(max-iter n 0))



(define (osterformel j)
  (let* ((a (modulo j 19))
        (b (modulo j 4))
        (c (modulo j 7))
        (k (floor (/ j 100)))
        (p (floor ( / (+ (* k 8) 13) 25)))
        (q (floor (/ k 4)))
        (M (modulo (+ 15 (- k p q)) 30))
        (N (modulo (+ 4 (- k q)) 7))
        (d (modulo (+ (* 19 a) M) 30))
        (e (modulo (+ (* 2 b) (* 4 c) (* 6 d) N) 7))
        (o (+ 22 d e))) o))


(define (n x) (+ x 1))



(define (sum x y)
  (if (= y 0)
      x
      (sum (n x) (- y 1))))



(define (mul x y)
  (if (= y 0) x (sum x (* x (- y 1)))))





