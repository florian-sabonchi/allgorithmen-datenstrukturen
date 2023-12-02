#lang racket



(define (euler n)
  (define (euler-iter quotient fak counter result n)
    (if (> n counter) (euler-iter (/ 1 fak) (* fak counter) (+ counter 1) (+ result quotient) n) result))
  (euler-iter 0 1 1.0 0 n))



(define (max-iter n m) (if (> (quotient n 10) 0) (max-iter (quotient n 10) (if (> (remainder n 10) m) (remainder n 10) m)) m))



(define (maxziffer n)(max-iter n 0))

(maxziffer 127456)



; 0



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






