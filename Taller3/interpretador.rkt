#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

;REPOSITORIO: https://github.com/TheCryss/FDLP-2023-1

;******************************************************************************************
;;;;; Interpretador Simple

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>          ::= <expression>
;;                          <un-programa (exp)>
;;  <expression>       ::= <numero>
;;                         <numero-lit (num)>
;;                     ::= "\""<texto>"\""
;;                         <texto-lit (txt)>
;;                     ::= <identificador>
;;                         <var-exp (id)>
;;                     ::= (<expresion> <primitiva-binaria> <expresion>)
;;                         <primapp-bin-exp (exp1 prim-binaria exp2)>
;;                     ::= <primitiva-unaria> (<expresion>)
;;                         <primapp-un-exp (prim-unaria exp)>
;;                     ::= Si <expresion> entonces <expresion> sino <expresion> finSI
;;                         <condicional-exp (test-exp true-exp false-exp)>
;;                     ::= declarar ({<identificador>=<expresion>}*(;)) {<expresion>}
;;                         <variableLocal-exp (ids exps cuerpo)
;;                     ::= procedimiento ({<identificador>}*(,)) haga <expresion> finProc
;;                         <procedimiento-exp (ids cuerpo)>
;;                     ::= evaluar <expresion> ({expresion}*(,)) finEval
;;                         <app-exp (rator rands)>
;;                     ::= letrec {<identificador> ({<identificador}*(,)) = <expresion>}* {<expresion>}
;;                         <letrec-exp (proc-names idss bodies letrec-body)
;;
;; <primitiva-binaria> ::= + (primitiva-suma)
;;                     ::= ~ (primitiva-resta)
;;                     ::= * (primitiva-div)
;;                     ::= concat (primitiva-concat)
;;
;; <primitiva-unaria>  ::= longitud (primitiva-longitud)
;;                     ::= add1 (primitiva-add1)
;;                     ::= sub1 (primitiva-sub1)

;******************************************************************************************

;******************************************************************************************
;Especificación léxica
(define scanner-spec-simple-interpreter
  '((white-sp ;Espacios en blanco
     (whitespace) skip) 
    (comentario ;Comentarios
     ("//" (arbno (not #\newline))) skip)
    (texto
     ((or letter "-") (arbno (or letter digit "-" ":"))) string)
    (identificador ;Identificadores
     ("@" (arbno letter)) symbol)
    (numero ;Número entero positivo
     (digit (arbno digit)) number)
    (numero ;Número entero negativo
     ("-" digit (arbno digit)) number)
    (numero
     (digit (arbno digit) "." digit (arbno digit)) number) ;Número decimal positivo
    (numero ;Número decimal negativo
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    ))

;Especificación Sintáctica (Gramática)
(define grammar-simple-interpreter
  '(;Programa
    (programa (expresion) un-programa)

    ;Expresiones
    (expresion (numero) numero-lit)
    (expresion ("\""texto"\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion
     ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)
    ;Condicional
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)
    ;Variables Locales
    (expresion ("declarar" "("  (separated-list identificador "=" expresion ";") ")" "{" expresion "}") variableLocal-exp)
    ;Procedimientos
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc" ) procedimiento-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)
    ;Recursividad
     (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "{" expresion "}") 
                letrec-exp)
     
    ;Primitivas-binarias
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)

    ;Primitivas-unarias
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    
    ))



;Construyendo datos automáticamente
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + Señal para lectura)
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
;Función que evalúa un programa teniendo en cuenta un ambiente dado (Se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                   (eval-expresion body (init-env))))))

;Ambiente inicial
;Inicializa y declara el ambiente con el que el interpretador comenzará
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;eval-expresion: <expresion> <environment> -> numero
;Evalua la expresion en el ambiente de entrada
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable env id))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let (
                             (args1 (eval-rand exp1 env))
                             (args2 (eval-rand exp2 env))
                             )
                         (apply-primitiva-binaria args1 prim-binaria args2)))
      (primapp-un-exp (prim-unaria exp)
                      (let (
                            (args (eval-rand exp env))
                            )
                        (apply-primitiva-unaria prim-unaria args)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (eval-expresion test-exp env))
                           (eval-expresion true-exp env)
                           (eval-expresion false-exp env)))
      (variableLocal-exp (ids exps cuerpo)
                          (let ((args (eval-rands exps env )))
                           (eval-expresion cuerpo (extend-env ids args env))
                           ))
       (procedimiento-exp (ids cuerpo)
        (cerradura ids cuerpo env)
      )
      (app-exp (rator rands)
               (let ((proc (eval-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expresion
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))))
  )
      
      


;Función auxiliar para aplicar eval-rand a cada elemento dentro de exp1 exp2
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;Función auxiliar para evaluar un "rand".
(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))

;apply-primitiva-binaria: <expresion> <primitiva> <expresion> -> numero or string
(define apply-primitiva-binaria
  (lambda (exp1 prim exp2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ exp1 exp2))
      (primitiva-resta () (- exp1 exp2))
      (primitiva-multi () (* exp1 exp2))
      (primitiva-div () (/ exp1 exp2))
      (primitiva-concat () (string-append exp1 exp2))
      )))

;apply-primitiva-unaria <primitiva> <expresion> -> numero
(define apply-primitiva-unaria
  (lambda (prim exp)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length exp))
      (primitiva-add1 () (+ exp 1))
      (primitiva-sub1 () (- exp 1))
      )))

;valor-verdad? <expresion> -> Boolean
;Evalua si un valor es #t, siendo #f si el valor es 0
(define valor-verdad?
  (lambda(x)
    (not (zero? x))))



;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?))
)


(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error "Error, la variable no existe"))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (buscar-variable old-env sym)))))))

;Función que se encarga de realizar cerraduras
(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb environment?)
   )
  )

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (eval-expresion body (extend-env ids args env))))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))


;************************************* PARTE A EVALAUAR ***********************************
;INCISO a)

;declarar (
;
;      @radio=2.5;
;      @areaCirculo= procedimiento (@radio) haga (3.14159265358979323846 *(@radio * @radio)) finProc
;
;     ) { 
;
;         evaluar @areaCirculo (@radio) finEval  
;
;       }

;INCISO b)

;letrec 
;       @factorial(@numero) = 
;                  Si @numero entonces (@numero * evaluar @factorial((@numero ~ 1)) finEval) sino 1 finSI
;        { evaluar @factorial (5) finEval }
;
;letrec 
;       @factorial(@numero) = 
;                  Si @numero entonces (@numero * evaluar @factorial((@numero ~ 1)) finEval) sino 1 finSI
;        { evaluar @factorial (10) finEval }


;INCISO c)

;letrec
;     @sumar(@a , @b) =
;            Si @a entonces evaluar @sumar(sub1(@a),add1(@b)) finEval sino @b finSI
;{ evaluar @sumar(3,4) finEval}

;INCISO d)

;letrec
;     @restar(@a , @b) =
;            Si @b entonces evaluar @restar(sub1(@a),sub1(@b)) finEval sino @a finSI
;
;{ evaluar @restar(10,3) finEval}
;
;
;letrec
;     @sumar(@a , @b) =
;            Si @a entonces evaluar @sumar(sub1(@a),add1(@b)) finEval sino @b finSI
;     @multiplicar(@a , @b) =
;            Si @a entonces evaluar @sumar( evaluar @multiplicar(sub1(@a), @b) finEval , @b) finEval sino @a finSI
;
;{ evaluar @multiplicar(10,3) finEval}


;INCISO e)
;letrec
;@integrantes() = "José-Sebastián-y-Sebastián"
;@saludar(@m) = ("Hola:" concat evaluar @m() finEval) 
;@decorate() = evaluar @saludar(@integrantes) finEval
;{evaluar @decorate() finEval}


;INCISO f)
;letrec
;@integrantes() = "José-Sebastián-y-Sebastián"
;@saludar(@m) = ("Hola:" concat evaluar @m() finEval)
;@decorate(@m) = (evaluar @saludar(@integrantes) finEval concat @m)
;{evaluar @decorate("-EstudiantesFLP") finEval}
;//Retorna "Hola:José-Sebastián-y-Sebastián-EstudiantesFLP"


;******************************************************************************************
(interpretador)
