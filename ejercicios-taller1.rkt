#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

; Procedimientos:

;; list-set :
;; Proposito:
;; L x n x X x P -> L' : Procedimiento que ingresa el elemento X en la
;; posicion ingresada n (indexando desde cero) solo si el elemento
;; original de la lista cumple con el predicado P.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define list-set (
    lambda (L n X P) 
        [cond
            [(eqv? n 0)
                (if (P (car L))
                    (cons X (cdr L))
                    (cons (car L) (cdr L))
                )
            ]
            [else
                (cons (car L) (list-set (cdr L) (- n 1) X P))
            ]
        ]
))

;; Pruebas
(list-set '(5 8 7 6) 2 '(1 2) odd?)
(list-set '(5 8 7 6) 2 '(1 2) even?)