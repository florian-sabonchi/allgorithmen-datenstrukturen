#lang racket

(define (sinus-approx x)
  (cond ((<= x 0.1) x)
        (else (- (* 3 (sinus-approx (/ x 3)))
                 (* 4 (expt (sinus-approx (/ x 3)) 3))))))




(define (get-digit-at x number)
  (modulo (quotient number (expt 10 x)) 10))



(define (isbn-mod isbn zaehler produkt)
  (cond ((= zaehler 0) (modulo produkt 11))
        (else (isbn-mod isbn (- zaehler 1)
                        (+ produkt (* (get-digit-at (- 9 zaehler) isbn) zaehler))))))




(define (isbn-test isbn) (if (= (isbn-mod isbn 9 0) 10) "X" (isbn-mod isbn 9 0)))

(isbn-test 392511825)


  

(define ( zylinder-kegel radius-zylinder hoehe-zylinder
                         radius-kegel hoehe-kegel)
  (define (volumen-zylinder radius hoehe)(* pi (expt radius 2) hoehe))
  (define (volumen-kegel radius hoehe)
  (* (/ 1 3) pi (expt radius 2)  hoehe))
  (/ (volumen-zylinder radius-zylinder hoehe-zylinder) (volumen-kegel radius-kegel hoehe-kegel)))

