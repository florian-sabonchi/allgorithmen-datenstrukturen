#lang racket

; Aufgabe 1

(define (count-item char liste counter)
  (if (pair? liste)
      (count-item char (cdr liste)
                  (if (eq? (car liste) char) (+ counter 1) counter))  counter))



(define (compress liste)
  (define (compress-helper liste result)    
      (if (pair? liste)
          (let ((c_char (car liste)))
            (compress-helper (cdr liste)
                             (if (memq c_char result)
                                 result
                                 (append result (list c_char) (list (count-item c_char liste 0)))))) result))
  (compress-helper liste (list )))


; Aufgabe 2

(define (expandiere-helper char counter result)
  (if (> counter 0)
      (expandiere-helper char (- counter 1) (append (list char) result)) result))


(define (expandiere sym-liste)
  (define (expandiere-iter liste result)
    (let ((c_list (if (and (pair? liste) (number? (car liste))) liste (append (list 1) liste))))
    (if (null? c_list) 
        result        
          (if (> (length c_list) 1)
              (expandiere-iter (cddr c_list)
                               (append result
                                       (expandiere-helper (car (cdr c_list)) (car c_list) '())))
              result))))

  (expandiere-iter sym-liste '()))


; Aufgabe 3

(define (loeschen liste n)
  (if (> n 0) (loeschen (cdr liste) (- n 1)) liste))





