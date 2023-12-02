#lang racket

; Aufgabe 1
 (define (zaehlen start ende n)
   (define (zahler-iter i ende n)
     (cond ((and (= (modulo i 3) 0) (= (modulo i 7) 0) (= n 1)) i)
           ((and (= (modulo i 3) 0) (= (modulo i 7) 0) (> n 1)) (zahler-iter (+ i 1) ende (- n 1)))
           ((> ende  i) (zahler-iter (+ i 1) ende n)) (else 0)))
   (zahler-iter start ende n))
 

; Aufgabe 2
(define (ziffer-pos zahl pos)
  (remainder (quotient zahl (expt 10 pos)) 10))


(define (gleiche-ziffern zahl) 
  (define (finde-zahl aktuelle-zahl)
    (cond ((= (ziffer-pos aktuelle-zahl 0) (ziffer-pos aktuelle-zahl (- (string-length (number->string zahl))
                                                                        (if (> aktuelle-zahl 0) 1 2)))) aktuelle-zahl)
          (else (finde-zahl (+ aktuelle-zahl 1))))) (finde-zahl zahl))



; Aufgabe 3
(define (konst-addierer n)
  (lambda (x) (+ x 1)))

(konst-addierer 1)





