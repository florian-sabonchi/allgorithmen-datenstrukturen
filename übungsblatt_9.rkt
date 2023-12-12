#lang racket

; Aufgabe 2

(define (liste-teilen eingabe)
  (define (liste-helfer liste zaehler liste-1 liste-2)
     (if (pair? liste)
         (liste-helfer (cdr liste) (+ zaehler 1)
                       (if (= (modulo zaehler 2) 0) (append liste-1 (list (car liste))) liste-1)
                       (if (> (modulo zaehler 2) 0) (append liste-2 (list (car liste))) liste-2))
         (append (list liste-1) (list liste-2)) ))
  (liste-helfer eingabe 0 null null))
 


; Aufgabe 3


(define (listen-verschmelzen eingabe)
  (define (liste-helfer liste-1 liste-2 ergebnis)
    (if (and (pair? liste-1) (pair? liste-2))
        (liste-helfer (cdr liste-1)
                      (cdr liste-2)
                      (append ergebnis (list (car liste-1)) (list (car liste-2)))) ergebnis))
  (liste-helfer (car eingabe) (cadr eingabe) null))



; Aufgabe 4

(define (hamming zahl-1 zahl-2)
  (define (haming-helper zahl-1 zahl-2 hamming-zahl)
    (if (and (pair? zahl-1) (pair? zahl-2))
        (haming-helper (cdr zahl-1) (cdr zahl-2)
                       (if (eq? (car zahl-1 ) (car zahl-2)) hamming-zahl (+ hamming-zahl 1))) hamming-zahl))
  (haming-helper zahl-1 zahl-2 0))

