#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

;;; 2. Funciones Parse y Unparse

;Función Parse
(define PARSEBNF
  (lambda (fnc)
    (cond
      [(null? fnc) '()]
      [(eq? (car fnc) 'FNC) (fnc-exp (cadr fnc) (and-clauses (PARSEBNF (cadr (caddr fnc)))))]
      [(eq? (car fnc) 'OR) (or-vars (or-list->varlist fnc))]
      [(eq? (caar fnc) 'OR) (cons (PARSEBNF (car fnc)) (PARSEBNF (cdr fnc)))]
      )))

; Ejemplos
(PARSEBNF (fnc-list 3 (and-list (list (or-list '(1 2 3)) (or-list '(1 3))))))
(PARSEBNF (fnc-list 2 (and-list (list (or-list '(-1 2)) (or-list '(2))))))
(PARSEBNF (fnc-list 4 (and-list (list (or-list '(-1 4)) (or-list '(2 3)) (or-list '(2 -3 -4))))))

;Función Unparse
;(define UNPARSEBNF
;  (lambda (tree)
;    (cond
;      [])
;    ))
