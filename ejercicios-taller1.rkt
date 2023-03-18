#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

; Procedimientos:

;; list-set :
;; Proposito:
;; L x n x X x P -> L' : Procedimiento que ingresa el elemento X en la
;; posicion ingresada n (indexando desde cero) de la lista L solo si el elemento
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
                    L
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


;; swapper :
;; Proposito:
;; E1 x E2 x L -> L' : Procedimiento que reemplaza cada ocurrencia anterior de E1
;; por E2 y cada ocurrencia anterior de E2 por E1 (Los elementos
;; E1 y E2 deben pertenecer a L) de la lista L
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define swapper
    (lambda (E1 E2 L)
        [cond
            [(null? L) empty]
            [else 
                (cond
                    [(eqv? (car L) E1)
                        (cons E2 (swapper E1 E2 (cdr L)))
                    ]
                    [(eqv? (car L) E2)
                        (cons E1 (swapper E1 E2 (cdr L)))
                    ]
                    [else
                        (cons (car L) (swapper E1 E2 (cdr L)))
                    ]
                )
            ]
        ]
    )
)

;; Pruebas
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))