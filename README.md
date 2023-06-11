# FDLP-2023-1
## Team members
- Jose Luis Hincapie Bucheli
- Sebatian Idrobo Avirama
- Juan Sebastian Getial Getial
-----------------------------

## Respuestas
### Pregunta 2
- Lista con los 10 elementos de valores denotados
    ```
    //Lista con los 10 elementos de valores denotados
    var
    @entero = 1
    @flotante = 1.01
    @hexadecimal = x16(14,1,0)
    @caracter = "a"
    @cadenacaracteres = "hola"
    @boolean = true
    @procedimiento = proc (@entrada) 1
    @lista = [1;2;3]
    @registro = {a = 1; b = 2; c = 3}
    @tupla = tupla[1;2;3]
    in
    begin
    [
    @entero;
    @flotante;
    @hexadecimal;
    @caracter;
    @cadenacaracteres;
    @boolean;
    @procedimiento;
    @lista;
    @registro;
    @tupla
    ]
    end
    ```
    
### Pregunta 3
- Manejo de variables actualizables
    ```
    //Manejo de variables actualizables
    //Retorna 1 indicando exito.
    var
    @x = 5
    in
    begin
    set @x = 10
    end
    ```
    
### Pregunta 4
- Creación de constante con un valor asignado retornando su valor
    ```
    //Creación de constante con un valor asignado retornando su valor
    const
    @x = 5
    in
    @x
    ```
- Creación de constante con un valor asignado retornando error al modificarla
    ```
    //Creación de constante con un valor asignado retornando error al modificarla
    const
    @x = 5
    in
    set @x = 10
    end
    ```
    
### Pregunta 5
- Primitivas aritméticas con enteros
    ```
    //Primitivas aritméticas con enteros
    var
    @suma = +(5,5)
    @resta = -(5,5)
    @multiplicacion = *(5,5)
    @residuo = %(12,5)
    @division = /(10,5)
    @add1 = add1(1)
    @sub1 = sub1(1)
    in
    [@suma;@resta;@multiplicacion;@residuo;@division;@add1;@sub1]
    ```
- Primitivas aritméticas con hexadecimales
    ```
    //No fueron realizadas
    ```
    
## Pregunta 6
- Procesar primitivas para booleanos
    ```
    //Procesar primitivas para booleanos
    var
    @menor = <(0,5)
    @mayor = >(5,0)
    @menorIgual = <=(0,5)
    @mayorIgual = >=(5,5)
    @igual = ==(1,0)
    @diferente = <>(1,0)
    @and = and(true,true)
    @or = or(true, false)
    @not = not(false)
    in
    [@menor;@mayor;@menorIgual;@mayorIgual;@igual;@diferente;@and;@or;@not]
    ```
    
## Pregunta 7
- Procesar primitivas para cadenas
    ```
    //Procesar primitivas para cadenas
    var
    @longitud = longitud("hola")
    @concatenar = concat("ho","la")
    in
    [@longitud;@concatenar]
    ```

### Pregunta 8
- Paso por referencia para registros y paso por valor de una variable
    ```
    var
    @x = [1;2;3]
    @y = 5
    @F1 = proc (@a) begin set-list(@a, 0, 0) end
    @F2 = proc (@a) begin set @a = 0 end
    in
    begin
    (@F1 @x);
    (@F2 @y);
    [@x;@y]
    end
    ```

### Pregunta 9
    ```
    rec 
        @lista-to-tupla(@l) =
            if vacio?(@l) then
                tupla[]
            else
                crear-tupla(cabeza(@l), (@lista-to-tupla cola(@l)))

        @factorial(@n) = 
            if <(@n, 2) then
                @n
            else
                *(@n, (@factorial sub1(@n)))

        @factorial-list(@l) = 
            if vacio?(@l) then
                []
            else
                crear-lista((@factorial cabeza(@l)), (@factorial-list cola(@l)))

        @registroFactorial(@l) =
            {valores = (@lista-to-tupla @l); factoriales = (@factorial-list @l)}

    in
        (@registroFactorial [1; 2; 3; 4; 7; 9])
    ```

### Pregunta 10
```
rec
    @map(@f, @l) =
        if vacio?(@l) then
            []
        else
            crear-lista((@f cabeza(@l)), (@map @f cola(@l)))
in
    var
        @doble = proc(@n) *(@n, @n)
    in
        (@map @doble [3; 1; 2; 5])
```

### Pregunta 11
- Punto a, for
```
var
    @l = []
in    
    begin
        for @i = 1 to 5 do
            append(@l, @i)
        done;
        @l
    end
```
- Punto b, while
```
var
    @esPar? = proc(@n)
                if >(%(@n, 2), 0) then
                    false
                else
                    true
    @l = []
    @i = 1
in
    begin
        while <(@i, 6) do
            begin
                append(@l, (@esPar? @i));
                set @i = add1(@i)
            end
        done;
        @l    
    end
```

 ### Pregunta 12

 - Declaracion de objetos y metodos
  ```
  class @interior_node extends @object
    field @left
    field @right
  def @init(@l, @r)
    begin
      set @left = @l;
      set @right = @r
    end
  def @str() "nodo"
  def @sum (@p) +(call @left.@sum(@p),call @right.@sum(@p))

class @leaf_node extends @object
    field @value
  def @init(@v) set @value = @v
  def @sum (@p) 
    (@p @value)
  def @str() "hoja"
  ```
  
  - Instancionado objetos y llamados a metodos

```
var
  @o1 = new @interior_node(
  new @interior_node(new @leaf_node(15),
  new @leaf_node(4)),
  new @leaf_node(5))
  @p = proc (@v) if >(@v,10) then @v else 0 
in
  call @o1.@sum(@p)
```
