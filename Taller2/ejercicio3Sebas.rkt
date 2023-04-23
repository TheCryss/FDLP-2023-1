#lang eopl
(require "ejercicio1.rkt")

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

;(define EVALUARSAT
;  (lambda (fnc)
;    (let
;        (
;         [listaPermutaciones (crearPermutaciones (listaInicial (fnc-list->var fnc)))]
;         [listaClausulasAnd (and-list->clausulas (fnclist->and fnc))]
;         )
;      (cond
;        [(null? fnc) #t]
;        [()])
;      
;      )))

(define evaluarCondiciones
  (lambda permutacion listaClausulasAnd)
  )

(define listaInicial
  (lambda (num)
    (if (> num 0) (cons #f (listaInicial (- num 1))) '())))

(define crearPermutaciones
  (lambda (listaInicial)
    (if (todosTrue? listaInicial) '() (cons listaInicial (cons (cambiarDigito listaInicial #f) (crearPermutaciones (cambiarDigito listaInicial #f)))))))
      

(define cambiarDigito
  (lambda (listaInicial cambiado?)
    (cond
      [(null? listaInicial) '()]
      [cambiado? (cons (car listaInicial) (cambiarDigito (cdr listaInicial) cambiado?))]
      [(not cambiado?)
       (if (eqv? (car listaInicial) #f)
           (cons #t (cambiarDigito (cdr listaInicial) #t))
           (cons #f (cambiarDigito (cdr listaInicial) #f)))])))

(define todosTrue?
  (lambda (lista)
    (cond
      [(null? lista) #t]
      [else (if (eqv? (car lista) #f) #f (todosTrue? (cdr lista)))])))

(define lista1 (list #f #f #f))