#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

;REPOSITORIO: https://github.com/TheCryss/FDLP-2023-1


;*****************************************************************************************
;;;;; Miny-Py

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>            ::= {<class-decl>}* <expression>
;;                           <a-program (c-decls exp)>
;;  <expression>         ::= <number>
;;                           <lit-exp (datum)>
;;                       ::= x <number> ({number}*)
;;                           <bignum-exp (base numbers)>
;;                       ::= "\""<texto>"\""
;;                           <texto-lit (txt)>
;;                       ::= <expr-bool>
;;                           <bool-exp (exp-bool)>
;;                       ::= <expr-lista>
;;                           <expr-lista-exp (expr-lista)>

;;                       ::= <expr-tupla>
;;                           <expr-tupla-exp (expr-tupla)>

;;                       ::= <unary-primitive-list (<expression>)
;;                           <unary-primitive-list-exp (un-prim expr)
;;                       ::= <list-primitive (<identifier>,{<expression>}*(,))>
;;                           <list-primitive-exp (bin-prim list-id rands)>
;;                       ::= <identifier>
;;                           <var-exp (id)>
;;                       ::= <primitive> ({<expression>}*(,))
;;                           <primapp-exp (prim rands)>
;;                       ::= if <expr-bool> then <expression> else <expression>
;;                           <if-exp (test-exp true-exp false-exp)>
;;                       ::= var {<identifier> = <expression>}* in <expression>
;;                           <let-exp (ids rands body)>
;;                       ::= const {<identifier> = <expression>}* in <expression>
;;                           <const-exp (ids rands body)>
;;                       ::= proc ({<identifier>}*(,))
;;                           <proc-exp (ids body)>
;;                       ::= (<expression> {<expression>}*)
;;                           <app-exp (rator rands)>
;;                       ::= rec {<identifier> ({<identifier}*(,)) = <expression>}* in <expression>
;;                           <letrec-exp (proc-names idss bodies letrec-body)>
;;                       ::= set <identifier> = <expression>
;;                           <varassign-exp (id rhs-exp)>
;;                       ::= begin <expression> {; <expression>}* end
;;                           <begin-exp (exp1 exps)>
;;                       ::= while <expr-bool> do <expression> done
;;                           <while-exp (expr-b expr)
;;                       ::= for <identifier> = <expression> <iterator> <expression> do <expression> done
;;                           <for-exp (i init iterator limit expr)>
;;
;;  <bool>               ::= true+
;;                           <true-bool>
;;                       ::= false
;;                           <false-bool>
;;
;;  <expr-bool>          ::= <bool>
;;                           <simple-expr-bool (bool)>
;;                       ::= <pred-prim> (<expression>,<expression>)
;;                           <pred-prim-expr-bool (pred-prim exp1 exp2)
;;                       ::= <oper-bin-bool> (<expr-bool>,<expr-bool>)
;;                           <oper-bin-bool-expr-bool (oper-bin-b expr-b1 expr-b2)>
;;                       ::= <oper-un-bool> (<expr-bool>)
;;                           <oper-un-bool-expr-bool (oper-un-b expr-b)
;;
;;  <oper-un-bool>       ::= not
;;                           <negation-oper-un-bool>
;;
;;  <oper-bin-bool>      ::= and
;;                           <and-oper-bin-bool>
;;                       ::= or
;;                           <or-oper-bin-bool>
;;
;;  <pred-prim>          ::= <
;;                           <smaller-than-pred-prim>
;;                       ::= >
;;                           <greater-than-pred-prim>
;;                       ::= <=
;;                           <less-equal-to-pred-prim>
;;                       ::= >=
;;                           <greater-equal-to-pred-prim>
;;                       ::= ==
;;                           <equal-to-pred-prim>
;;                       ::= <>
;;                           <not-equal-to-pred-prim>
;;
;;  <expr-lista>         ::= [{<expression>}*(;)]
;;                           <simple-expr-lista (exps)>
;;
;;  <expr-tupla>         ::= tupla[{<expression>}*(;)]
;;                           <simple-expr-tupla (exps)
;;
;;  <expr-registro>      ::= {<identificador> = <expresion>}+(;)
;;                           <simple-expr-tupla (ids exps)>
;;
;;  <unary-primitive-list> ::=vacio?
;;                            <is-null-primitive>
;;                        ::= vacio
;;                            <null-primitive>
;;                        ::= lista?
;;                            <is-lista-primitive>
;;                        ::= cabeza
;;                            <car-primitive>
;;                        ::= cola
;;                            <cdr-primitive>
;;                        ::= append
;;                            <append-primitive>
;;                        ::= ref-list
;;                            <ref-list-primitive>
;;                        ::= set-list
;;                            <set-list-primitive>
;;
;;  <iterator>           ::= to
;;                           <to-iterator>
;;                       ::= downto
;;                           <downto-iterator>
;;  <primitive>          ::= +
;;                           <add-prim>
;;                       ::= -
;;                           <substract-prim>
;;                       ::= *
;;                           <mult-prim>
;;                       ::= add1
;;                           <incr-prim>
;;                       ::= sub1
;;                           <decr-prim>
;;                       ::= zero?
;;                           <zero-test-prim>
;;
;; TO-DO: OOP

;******************************************************************************************


;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)                                      ;Espacios en blanco
    (comment ("%" (arbno (not #\newline))) skip)                        ;Comentarios
    (identifier
      ("@" letter (arbno (or letter digit "_" "-" "?")))                ;Identificadores
      symbol)
    (number (digit (arbno digit)) number)                               ;Entero positivo
    (number ("-" digit (arbno digit)) number)                           ;Entero negativo
    (number (digit (arbno digit) "." digit (arbno digit)) number)       ;Decimal positivo
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)   ;Decimal negativo
    (texto
     ((or letter "-") (arbno (or letter digit "-" ":"))) string)        ;Cadena de texto
  )
)

(define the-grammar
  '(
    ;Programa
    (program ((arbno class-decl) expression) a-program)
    
    ;Número en base distinta a 10
    (expression ("x" number "(" (arbno number) ")") bignum-exp)

    ;Identificador
    (expression (identifier) var-exp)

    ;Definiciones
    (expression
      ("var" (arbno  identifier "=" expression) "in" expression)
      let-exp)
   (expression
      ("const" (arbno  identifier "=" expression) "in" expression)
      const-exp)
   (expression                         
      ("rec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)

    ;Datos
    (expression (number) lit-exp)
    (expression ("\""texto"\"") texto-lit)
    (bool ("true") true-bool)
    (bool ("false") false-bool)

    ;Constructores de datos predefinidos
        ;Listas
    (expr-lista ("[" (separated-list expression ";") "]") simple-expr-lista)
    (expression (expr-lista) expr-lista-exp)

        ;Tuplas
    (expr-tupla ("tupla[" (separated-list expression ";") "]") simple-expr-tupla)
    (expression (expr-tupla) expr-tupla-exp)

        ;Registros
    (expr-registro ("{" (separated-list identifier "=" expression ";") "}") simple-expr-registro)
    (expression (expr-registro) expr-registro-exp)

    ;(expr-registro ("{" identifier "=" expression ";}"))
    ;(expr-registro ("{" identifier "=" expression ";"  (separated-list (identifier "=" expression) ";") "}"))
    ;(expression (expr-registro) expr-registro-exp)


        ;Expresiones booleanas
    (expr-bool (bool) simple-expr-bool)
    (expr-bool (pred-prim "(" expression "," expression ")") pred-prim-expr-bool)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") oper-bin-bool-expr-bool)
    (expr-bool (oper-un-bool "(" expr-bool ")") oper-un-bool-expr-bool)
    (expression (expr-bool) bool-exp)

        ;pred-prim
    (pred-prim ("<") smaller-than-pred-prim)
    (pred-prim (">") greater-than-pred-prim)
    (pred-prim ("<=") less-equal-to-pred-prim)
    (pred-prim (">=") greater-equal-to-pred-prim)
    (pred-prim ("==") equal-to-pred-prim)
    (pred-prim ("<>") not-equal-to-pred-prim)

        ;oper-bin-bool
    (oper-bin-bool ("and") and-oper-bin-bool)
    (oper-bin-bool ("or") or-oper-bin-bool)

        ;oper-un-bool
    (oper-un-bool ("not") negation-oper-un-bool)

    ;Estructuras de control
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)
    (expression
      ("if" expr-bool "then" expression "else" expression)
      if-exp)
    (expression ("while" expr-bool "do" expression "done") while-exp)
    (expression ("for" identifier "=" expression iterator expression "do" expression "done") for-exp)

        ;iterator
    (iterator ("to") to-iterator)
    (iterator ("downto") downto-iterator)

    ;Primitivas aritméticas para enteros
    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (primitive ("zero?") zero-test-prim) ;Y esto de donde salió?

    ;Primitivas aritméticas para hexadecimales
    ;TO-DO

    ;Primitivas sobre cadenas
    ;TO-DO

    ;Primitivas sobre listas
    (unary-primitive-list ("vacio?") is-null-primitive-list)
    (unary-primitive-list ("vacio") null-primitive-list)
    ;TO-DO: Realizar el unary-primitive-list de "crear-lista"
    (unary-primitive-list ("lista?") is-lista-primitive)
    (unary-primitive-list ("cabeza") car-primitive-list)
    (unary-primitive-list ("cola") cdr-primitive-list)
    (list-primitive ("append") append-primitive)
    (list-primitive ("ref-list") ref-list-primitive)
    (list-primitive ("set-list") set-list-primitive)
    (expression (unary-primitive-list "(" expression ")") unary-primitive-list-exp)
    (expression (list-primitive "(" identifier "," (separated-list expression ",") ")") list-primitive-exp)

    ;Primitivas sobre tuplas
    (unary-primitive-tuple ("vacio-tupla?") is-null-primitive-tuple)
    (unary-primitive-tuple ("vacio-tupla") null-primitive-tuple)
    ;TO-DO: Realizar el unary-primitive-tuple de "crear-tupla"
    (unary-primitive-tuple ("tupla?") is-tuple-primitive)
    (unary-primitive-tuple ("cabeza-tupla") car-primitive-tuple)
    (unary-primitive-tuple ("cola-tupla") cdr-primitive-tuple)
    (tuple-primitive ("ref-tuple") ref-tuple-primitive)
    (expression (unary-primitive-tuple "(" expression ")") unary-primitive-tuple-exp)
    (expression (tuple-primitive "(" identifier "," (separated-list expression ",") ")") tuple-primitive-exp)
    

    ;Definición/Invocación de procedimientos
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)

    ;Definición/invocación de procedimientos recursivos
    ;TO-DO

    ;Variables actualizables (mutables)
    (expression ("set" identifier "=" expression) varassign-exp)

    ;Secuenciación
    (expression
      ("(" expression (arbno expression) ")")
      app-exp) ;??????????????

    ;Iteración
    ;TO-DO


;^;;;;;;;;;;;;;;; POO ;;;;;;;;;;;;;;;;

    (class-decl                         
      ("class" identifier 
        "extends" identifier                   
         (arbno "field" identifier)
         (arbno method-decl)
         )
      a-class-decl)

    (method-decl
      ("method" identifier 
        "("  (separated-list identifier ",") ")" ; method ids
        expression 
        )
      a-method-decl)
    (expression ("mostrar") mostrar-exp)

    (expression 
      ("new" identifier "(" (separated-list expression ",") ")")
      new-object-exp)

    (expression
      ("send" expression identifier
        "("  (separated-list expression ",") ")")
      method-app-exp)

    (expression                                
      ("super" identifier    "("  (separated-list expression ",") ")")
      super-call-exp)

        ))
;;;;;;;;;;;;;;;;; POO FIN ;;;;;;;;;;;;;;;;


(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define list-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;^;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
        (elaborate-class-decls! c-decls) ;\new1
        (eval-expression exp (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (mostrar-exp () the-class-env)
      
      (lit-exp (datum) datum)
      
      ; bignum
      (bignum-exp (base numbers) (eval-bignum-exp base numbers))
      
      ; Cadena
      (texto-lit (txt) txt)
      
      ; Booleano
      (bool-exp (exp-bool) (eval-expr-bool exp-bool env))
      
      ; Lista
      (expr-lista-exp (expr-lista) (eval-expr-lista expr-lista env))

      ; Tupla
      (expr-tupla-exp (expr-tupla) (eval-expr-tupla expr-tupla env))

      ;Registro
      (expr-registro-exp (expr-registro) (eval-expr-registro expr-registro env))
      
      ; Primitivas unarias
      (unary-primitive-list-exp (un-prim expr) (apply-unary-primitive-list 
                                                un-prim 
                                                (eval-expression expr env)))
      
      (list-primitive-exp (bin-prim list-id rands) (apply-list-primitive
                                                bin-prim
                                                (apply-env-ref env list-id)
                                                (eval-rands rands env)))

      (unary-primitive-tuple-exp (un-prim expr) (apply-unary-primitive-tuple
                                                un-prim 
                                                (eval-expression expr env)))

      (tuple-primitive-exp (bin-prim list-id rands) (apply-tuple-primitive
                                                bin-prim
                                                (apply-env-ref env list-id)
                                                (eval-rands rands env)))
      
      (var-exp (id) (apply-env env id))
      
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      
      (if-exp (test-exp true-exp false-exp)
        (if (eval-expr-bool test-exp env)
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      
      (let-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      
      (const-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env-constant ids args env))))
      
      (proc-exp (ids body)
        (closure ids body env))
      
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env))
              (args (eval-rands      rands env)))
          (if (procval? proc)
            (apply-procval proc args)
            (eopl:error 'eval-expression 
              "Attempt to apply non-procedure ~s" proc))))
      
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      
      (varassign-exp (id rhs-exp)
        (setref!
          (apply-env-ref env id)
          (eval-expression rhs-exp env))
        1)
;&
      (begin-exp (exp1 exps)
        (let loop ((acc (eval-expression exp1 env))
                   (exps exps))
          (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))
      
      (while-exp (expr-b expr) 
        (let loop ((condition expr-b)
                    (expr-e expr))
          (if (eval-expr-bool condition env) 
            (loop condition (eval-expression expr env))
            1)))
      
      (for-exp (i init iterator limit expr) 
        (let ((lim (eval-expression limit env))
              (ini (eval-expression init env))
              (iter (apply-iterator iterator)))
          (let loop ((val-i ini)
                      (expr-e expr))
            (if (eqv? val-i lim)
              (eval-expression expr (extend-env (list i) (list val-i) env))
              (loop (+ val-i iter) (eval-expression expr (extend-env (list i) (list val-i) env)))))))
      
;^;;;;;;;;;;;;;;; begin new cases for chap 5 ;;;;;;;;;;;;;;;;
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            'initialize class-name obj args)
          obj))
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))
      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))
;^;;;;;;;;;;;;;;; end new cases for chap 5 ;;;;;;;;;;;;;;;;
      )))

(define apply-iterator
  (lambda (it)
    (cases iterator it
      (to-iterator () 1)
      (downto-iterator () -1)
    )
  )
)      

(define eval-rands
  (lambda (exps env)
    (map
      (lambda (exp) (eval-expression exp env))
      exps)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      (zero-test-prim () (if (zero? (car args)) 1 0))
      ;(list-prim () args)               ;already a list
      ;(nil-prim () '())
      ;(car-prim () (car (car args)))
      ;(cdr-prim () (cdr (car args)))
      ;(cons-prim () (cons (car args) (cadr args)))
      ;(null?-prim () (if (null? (car args)) 1 0))
      )))

(define apply-unary-primitive-list
  (lambda (un-prim arg)
    (cases unary-primitive-list un-prim
      (is-null-primitive-list ()
        (cases lista arg
          (lista-vacia () #t)
          (else #f)
        )
      )
      
      (null-primitive-list () lista-vacia)
      
      (is-lista-primitive () (lista? arg))
      
      (car-primitive-list () 
        (cases lista arg
          (lista-vacia () (eopl:error 'apply-unary-primitive-list
                            "List index out of range"))
          (lista-extendida (vals) (vector-ref vals 0))
        )
      )
      
      (cdr-primitive-list () 
        (cases lista arg
          (lista-vacia () (eopl:error 'apply-unary-primitive-list
                            "List index out of range"))
          (lista-extendida (vals) 
            (letrec ((vals-l (vector->list vals))
                      (cdr-vals-l (cdr vals-l)))
              (if (null? cdr-vals-l)
                lista-vacia
                (lista-extendida (list->vector cdr-vals-l)))))
        )
      )
    )
  )
)

(define apply-list-primitive
  (lambda (l-prim list-ref rands)
    (let ((l (deref list-ref))
          (val (car rands))
          (pos (cadr rands)))
      (cases list-primitive l-prim
        (append-primitive () 
          (let ((new-list 
                  (cases lista l
                    (lista-vacia () (lista-extendida (vector val)))
                    (lista-extendida (vals) (letrec ((vals-l (vector->list vals))
                                                      (new-vals (append vals-l (list val))))
                                              (lista-extendida (list->vector new-vals))))
                  )))
            (setref! list-ref new-list)))
        (ref-list-primitive () 
          (cases lista l
            (lista-vacia () (eopl:error 'apply-list-primitive
                            "List index out of range"))
            (lista-extendida (vals) (vector-ref vals val))
          )
        )
        (set-list-primitive ()
          (cases lista l
                    (lista-vacia () (eopl:error 'apply-list-primitive
                            "List index out of range"))
                    (lista-extendida (vals) (vector-set! vals pos val))
                  )
        )
      ))
  )
)

(define apply-unary-primitive-tuple
  (lambda (un-prim arg)
    (cases unary-primitive-tuple un-prim
      (is-null-primitive-tuple ()
                               (cases tupla arg
                                 (tupla-vacia () #t)
                                 (else #f))
                               )
      
      (null-primitive-tuple () tupla-vacia)
      
      (is-tuple-primitive () (tupla? arg))
      
      (car-primitive-tuple () 
                           (cases tupla arg
          (tupla-vacia () (eopl:error 'apply-unary-primitive-tuple
                                      "Tuple index out of range"))
          (tupla-extendida (vals) (vector-ref vals 0))
          )
        )
      
      (cdr-primitive-tuple () 
                           (cases tupla arg
          (tupla-vacia () (eopl:error 'apply-unary-primitive-tuple
                                      "Tuple index out of range"))
          (tupla-extendida (vals) 
                           (letrec ((vals-l (vector->list vals))
                     (cdr-vals-l (cdr vals-l)))
              (if (null? cdr-vals-l)
                  tupla-vacia
                (tupla-extendida (list->vector cdr-vals-l)))))
          )
        )
      )
    )
  )

(define apply-tuple-primitive
  (lambda (t-prim tuple-ref rands)
    (lambda (t-prim tuple-ref rands)
      (let ((t (deref tuple-ref))
          (val (car rands))
          (pos (cadr rands)))
      
      (cases tuple-primitive t-prim
        (ref-tuple-primitive () 
                             (cases tupla t
            (tupla-vacia () (eopl:error 'apply-list-primitive
                                        "Tuple index out of range"))
            (tupla-extendida (vals) (vector-ref vals val))
            )
          ))
      )))
  )


(define eval-bignum-exp
  (lambda (base numbers)
    [cond
      [(null? numbers)
        0
      ]
      [else
        (eval-bignum base numbers 0)
      ]
    ]
  )
)

(define eval-bignum
  (lambda (base numbers init)
    [cond
      [(null? numbers)
        0
      ]
      [else
        (let
          (
            (n (* (car numbers) (expt base init)))
          )
          (+ n (eval-bignum base (cdr numbers) (+ init 1)))
        )
      ]
    ]
  )
)

(define-datatype lista lista?
  (lista-vacia)
  (lista-extendida
    (vals vector?)
  )
)

(define-datatype tupla tupla?
  (tupla-vacia)
  (tupla-extendida
   (vals vector?)
   )
  )

(define-datatype registro registro?
  (registro-extendido (keys vector?) (vals vector?)))

;(expr-registro ("{" identifier "=" expression ";" (separated-list (identifier "=" expression) ";") "}"))
;(expression (expr-registro) expr-registro-exp)

(define eval-expr-lista
  (lambda (expr-l env)
    (cases expr-lista expr-l
      (simple-expr-lista (exps)
        (let ((vals (eval-rands exps env)))
        (if (null? vals)
          (lista-vacia)
          (lista-extendida (list->vector vals))))
      )
    )
  )
)

(define eval-expr-tupla
  (lambda (expr-t env)
    (cases expr-tupla expr-t ;En caso de que expr-t sea una expr-tupla
      (simple-expr-tupla (exps)
                         (let ((vals (eval-rands exps env)))
                           (if (null? vals)
                             (tupla-vacia)
                             (tupla-extendida (list->vector vals)))))
      )
    )
  )

(define eval-expr-registro
  (lambda (expr-r env)
    (cases expr-registro expr-r
      (simple-expr-registro (ids args)
                            (let ((vals (eval-rands args env)))
                              (registro-extendido (list->vector ids) (list->vector vals))))
      )
    ))

(define init-env 
  (lambda ()
    (extend-env
      '(@i @v @x)
      '(1 5 10)
      (empty-env))))


;^;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define eval-bool
  (lambda (b)
    (cases bool b
      (true-bool () #t)
      (false-bool () #f)
    )
  )
)

(define eval-expr-bool
  (lambda (exp-bool env)
    (cases expr-bool exp-bool
      (simple-expr-bool (bool) (eval-bool bool))
      (pred-prim-expr-bool (pred-prim exp1 exp2)
        (eval-pred-prim pred-prim exp1 exp2 env)
      )
      (oper-bin-bool-expr-bool (oper-bin-b expr-b1 expr-b2)
        (let ((expr1 (eval-expr-bool expr-b1 env))
              (expr2 (eval-expr-bool expr-b2 env)))
              (eval-oper-bin-bool oper-bin-b expr1 expr2))
      )
      (oper-un-bool-expr-bool (oper-un-b expr-b)
        (let ((expr (eval-expr-bool expr-b env)))
              (eval-oper-un-bool oper-un-b expr))
      )
    )
  )
)

(define eval-oper-un-bool
  (lambda (oper-un-b expr)
    (cases oper-un-bool oper-un-b
      (negation-oper-un-bool () (not expr))
    )
  )
)

(define eval-oper-bin-bool
  (lambda (oper-bin-b expr1 expr2)
    (cases oper-bin-bool oper-bin-b
      (and-oper-bin-bool () (and expr1 expr2))
      (or-oper-bin-bool () (or expr1 expr2))
    )
  )
)

(define eval-pred-prim
  (lambda (pred-p exp1 exp2 env)
    (cases pred-prim pred-p
      (smaller-than-pred-prim () 
        (< (eval-expression exp1 env) (eval-expression exp2 env))
      )
      (greater-than-pred-prim () 
        (> (eval-expression exp1 env) (eval-expression exp2 env))
      )
      (less-equal-to-pred-prim () 
        (<= (eval-expression exp1 env) (eval-expression exp2 env))
      )
      (greater-equal-to-pred-prim () 
        (>= (eval-expression exp1 env) (eval-expression exp2 env))
      )
      (equal-to-pred-prim () 
        (eqv? (eval-expression exp1 env) (eval-expression exp2 env))
      )
      (not-equal-to-pred-prim () 
        (not (eqv? (eval-expression exp1 env) (eval-expression exp2 env)))
      )
    )
  )
)

(define true-value?
  (lambda (x)
    (not (zero? x))))


;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;


(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))
        
;^;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype procval procval?
  (closure 
    (ids (list-of symbol?)) 
    (body expression?)
    (env environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
        (eval-expression body (extend-env ids args env))))))
               
;^;;;;;;;;;;;;;;; references ;;;;;;;;;;;;;;;;

(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?))
  (a-const
    (position integer?)
    (vec vector?)))

(define deref 
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos))
      (a-const (pos vec)
              (vector-ref vec pos)))))

(define setref! 
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
        (vector-set! vec pos val))
      (a-const (pos vec)
        (eopl:error 'setref! "No se puede cambiar el valor de una constante")))
    1))

;^;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)              ; can use this for anything.
    (env environment?))
  (extended-env-constant
    (syms (list-of symbol?))
    (vals vector?)              ; can use this for anything.
    (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define extend-env-constant
  (lambda (syms vals env)
    (extended-env-constant syms (list->vector vals) env)))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-ref pos vals)
              (apply-env-ref env sym))))
      (extended-env-constant (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-const pos vals)
              (apply-env-ref env sym)))))))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

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

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))


;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))

;^; waiting for 5-4-2.  Brute force code.
(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))


;; evaluar
(define aux
   (lambda (x)
     x))

(define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'object)
      '()
      (let ((c-decl (lookup-class class-name)))
        (cons
          (make-first-part c-decl)
          (new-object (class-decl->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (class-decl->class-name c-decl)
      (make-vector (length (class-decl->field-ids c-decl))))))

;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

;;; methods are represented by their declarations.  They are closed
;;; over their fields at application time, by apply-method.

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'object)
      (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
          (apply-method m-decl host-name self args)
          (find-method-and-apply m-name 
            (class-name->super-name host-name)
            self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
      parts
      (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-expression body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (build-field-env 
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-env)
      (extend-env-refs
        (part->field-ids (car parts))
        (part->fields    (car parts))
        (build-field-env (cdr parts))))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

;; find a method in a list of method-decls, else return #f

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))
      
;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of class-decls.

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Unknown class ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; selectors of all sorts ;;;;;;;;;;;;;;;;

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))

;;

(define read-eval-print 
  (sllgen:make-rep-loop  "-->" eval-program
                         (sllgen:make-stream-parser
                                  the-lexical-spec 
                                  the-grammar)))

(read-eval-print)

;;Ejemplos 
;; class c1 extends object  field x field y  method initialize()  begin set x = 1; set y = 2 end method m1() x method m2() y  let o1 = new c1() in send o1 m1()


;;;; class c1 extends object  field x field y  method initialize()  begin set x = 1; set y = 2 end method m1() x method m2() y  class c2 extends c1  field x field y  method initialize()  begin set x = 2; set y = 3 end method m1() x  let o1 = new c1() o2 = new c2() in send o2 m2()


;;;; class c1 extends object  field x field y  method initialize()  begin   set x = 1; set y = 2 end method m1() x method m2() y  class c2 extends c1  field x field y  method initialize()  begin   super initialize(); set  x = 2; set y = 3 end method m1() x  let o1 = new c1() o2 = new c2() in send o2 m2()

;;class c1 extends object  field x field y  method initialize()  begin   set x = 1; set y = 2 end method m1() x method m2() send self m1()  class c2 extends c1  field x field y  method initialize()  begin   super initialize(); set  x = 9; set y = 10 end method m1() x  let o1 = new c1() o2 = new c2() in send o2 m2()
