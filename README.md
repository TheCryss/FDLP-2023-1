# FDLP-2023-1
## Team members
- Jose Luis Hincapie Bucheli
- Sebatian Idrobo Avirama
- Juan Sebastian Getial Getial
-----------------------------

## Respuestas

### Pregunta 4
- Crear una constante con algún valor asignado y retornar su valor
    ```
    const
        @c = 1
    in
        @c
    ```
- Crear una constante con algún valor asignado y retornar un error al tratar de modificar su valor
    ```
    const
        @c = 1
    in
        begin
            set @c = 10;
            @c
        end
    ```

### Pregunta 8
- Paso por referencia para registros
    ```
    var
    @x = {hola = 1; adios = 3; jeje = 5}
    @y = proc (@A) begin set-registro(@A, "hola", 0) end
    in
    begin
    (@y @x);
    ref-registro(@x, "hola")
    end
    ```
- Paso por referencia para las listas
    ```
    var
    @x = [1;2]
    @y = proc (@A) begin set-list(@A, 0, 0) end
    in
    begin
    (@y @x);
    ref-list(@x, 0)
    end
    ```
- Paso por valor de una variable
    ```
    var
    @x = 5
    @y = proc (@a) begin set @a = 0 end
    in
    begin
    (@y @x);
    @x
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
