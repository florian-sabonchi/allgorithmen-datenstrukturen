#lang racket

; Aufgabe 1

(define (werte-aus term zuweisung)
    (define (werte-helper c_zuweisung temp)
    (let* ((term_symbol (car term))
          (operation_symbol (cond ((eq? '* term_symbol) *)
                                   ((eq? '+ term_symbol) +)
                                   ((eq? '/ term_symbol) /)
                                   ((eq? '- term_symbol) -))))
    (if (pair? c_zuweisung)
        (werte-helper (cdr c_zuweisung)
                      (append temp (cdr (car c_zuweisung)))) (apply operation_symbol (append temp (filter number?  term))))))
  (werte-helper zuweisung '()))


; (werte-aus '(/ a b) '((a 6) (b 3))) 


; (define (werte-aus term zuweisung)
;    (let* ((term_symbol (car term))
;          (operation (cond ((eq? '* term_symbol) *)
;                                   ((eq? '+ term_symbol) +)
;                                   ((eq? '/ term_symbol) /)
;                                   ((eq? '- term_symbol) -))))
;      (apply operation (filter number?  (flatten (append term zuweisung))))))


(werte-aus '(* x ) '((x 3) (y 5)))