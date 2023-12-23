#lang racket

(define (removeFirstLast string)
  (list->string (drop-right (cdr (string->list string)) 1)))



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
  
 
   



(sicheresPasswort "ABC123") 