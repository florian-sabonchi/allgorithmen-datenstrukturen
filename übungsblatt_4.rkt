#lang racket

(define (ganzzahlige-wurzel? n)(integer? (sqrt n)))

(define (pruefung n a)
  (let ((b (sqrt (- (expt a 2) n))))
  (if (not (integer? b)) (pruefung n (+ a 1)) (- a b))))

(define (fakt n)
  (pruefung n (ceiling (sqrt n))))


(define (primzahl-test n i)
  (cond ((= (modulo n i) 0) #f)
         ((< i (- n 1)) (primzahl-test n (+ 1 i))) (else #t)))


(define (primzahl? n)(primzahl-test n 2))


(define (berrechne-kubiksumme x y z) (if (< z 3)
                                         (berrechne-kubiksumme (* x y) x (+ z 1)) x))


(define (finde-quersumme n q) (if (> n 0)
                                  (finde-quersumme (quotient n 10) (+ q (remainder n 10)))
                                  (berrechne-kubiksumme q q 1)))


(define (kubiksumme n)(finde-quersumme n 0))


(define (get-at-pos n pos)
  (cond ((>= pos 0) (remainder (quotient n (expt 10 pos)) 10))))  


(define (find-caesar n k r i)
  (let ((n-at-pos (get-at-pos n i)))
    (if (>= i 0) (find-caesar n k (string-append r (cond ((>= (+ n-at-pos k) 10) (number->string (remainder (+ n-at-pos k) 10)))
                                                         (else (number->string (+ n-at-pos k))))) (- i 1)) (string->number r))))


(define (caesar_encrypt n k)
  (find-caesar n k "" (- (string-length (number->string n)) 1)))
  
