#lang racket

; Aufgabe 1

(define (loesche liste praedikat)
  (if (and (pair? liste) (praedikat (car liste)))
      (loesche (cdr liste) praedikat) liste)) 


; Aufgabe 2


(define (get-at list index)
  (if (> index 0) (get-at (cdr list) (- index 1)) (car list)))

 (define (drehe liste)
  (define (drehe-helper index ergebniss)
    (if (>= index 0) (drehe-helper (- index 1)
                                  (append ergebniss (list (get-at liste index)) )) ergebniss))
   (drehe-helper (- (length liste) 1) (list )))

; Aufgabe 3

(define (typ-or typ-1 typ-2)
  (lambda (x) (if (or typ-1 x typ-2 x) #t #f)))


(define paar-oder-liste? (typ-or pair? list?))
(define integer-oder-boolean? (typ-or integer? boolean?))


; Aufgabe 4

(define (operation operatoren n)
  (define (get-item liste index)
    (if (= index 1)
         (car liste) (get-item (cdr liste) (- index 1))))
  (lambda (zahlen) (apply (get-item operatoren n) zahlen)))

; Aufgabe 5

(define (caesar_encrypt_list data key)
  (define (caesar_encrypt_helper c_data c_key)
    (if (null? c_data) (list )
        (cons (modulo (+ (car c_data )  (car c_key)) 10)
              (caesar_encrypt_helper (cdr c_data) (if (pair? (cdr c_key)) (cdr c_key) key)))))
  (caesar_encrypt_helper data key))