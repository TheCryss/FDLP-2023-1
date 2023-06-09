# FDLP-2023-1
## Team members
- Jose Luis Hincapie Bucheli
- Sebatian Idrobo Avirama
- Juan Sebastian Getial Getial
-----------------------------

## Respuestas
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
