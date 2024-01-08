#lang racket

; Aufgabe 1
(define (removeFirstLast string)
  (list->string (drop-right (cdr (string->list string)) 1)))


; Aufgabe 2

(define (sicheresPasswort passwort)
  (define (passwort-helfer passwort-liste kelein-buchstaben groes-buchstabe andere-zeichen)
    (if (pair? passwort-liste)
        (let ((aktuelles_zeichen (car passwort-liste)))
          (cond ((and (char-alphabetic? aktuelles_zeichen) (eq? aktuelles_zeichen (char-downcase aktuelles_zeichen)))
                 (passwort-helfer (cdr passwort-liste) (+ kelein-buchstaben 1) groes-buchstabe andere-zeichen))
                ((and (char-alphabetic? aktuelles_zeichen) (eq? aktuelles_zeichen (char-upcase aktuelles_zeichen)))
                 (passwort-helfer (cdr passwort-liste) kelein-buchstaben (+ groes-buchstabe 1) andere-zeichen))
                (else  (passwort-helfer (cdr passwort-liste) kelein-buchstaben groes-buchstabe  (+ andere-zeichen 1)))))
        (and (>= kelein-buchstaben 1) (>= groes-buchstabe 1) (>= andere-zeichen 2) (>= (length (string->list passwort))) )))
  (passwort-helfer (string->list passwort) 0 0 0))
  
 

; Aufgabe 3
 (define (vektor-add . vektoren)
  (define (vektor-helper c_vektoren result)
    (if (> (length c_vektoren) 1)
        (vektor-helper (cdr c_vektoren) (map + result (cadr c_vektoren))) result))
  (vektor-helper vektoren  (car vektoren)))



