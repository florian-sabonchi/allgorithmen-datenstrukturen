#lang racket

; Aufgabe 1

(define (zaehle-symbol liste symbol zaehler)
  (if (and (pair? liste) (eq? (car liste) symbol))
      (zaehle-symbol (cdr liste) symbol (+ zaehler 1))
      (list zaehler liste)))


(define (compress liste)
  (define (compress-iter c_liste ergebniss)
    (let* ((symbol (if (pair? c_liste) (car c_liste) null)) (zaehle-ergebniss (zaehle-symbol c_liste symbol 0)))
      (if (pair? c_liste)
          (compress-iter (flatten (cdr zaehle-ergebniss))
                         (append ergebniss (if (> (car zaehle-ergebniss) 1)
                                               (list (car zaehle-ergebniss) (car c_liste)) (list (car c_liste))))) ergebniss)))
  (compress-iter liste null))


; Aufgabe 2

(define (expandiere-helper char counter result)
  (if (> counter 0)
      (expandiere-helper char (- counter 1) (append (list char) result)) result))


(define (expandiere sym-liste)
  (define (expandiere-iter liste result)
    (let ((c_list (if (and (pair? liste) (number? (car liste))) liste (append (list 1) liste))))
      (if (> (length c_list) 1)
              (expandiere-iter (cddr c_list)
                               (append result
                                       (expandiere-helper (car (cdr c_list)) (car c_list) '())))
              result)

      ))(expandiere-iter sym-liste '()))

  


; Aufgabe 3

(define (loeschen liste n)
  (if (> n 0) (loeschen (cdr liste) (- n 1)) liste))




