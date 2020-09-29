# Mini-Compiler
__[ CC5116 ] - Diseño e Implementación de Compiladores__.

Sergio Morales & Bryan Ortiz.

## Entrega 1
Para la entrega 1, se implementan las siguientes especificaciones:
- [x] ``let``-bindings.
- [x] Aritmética de enteros ( ``+ - * /`` ).
- [x] Valores booleanos ( ``true``, ``false`` ).
- [x] Aritmética de booleanos ( ``and``, ``or``, ``not`` ).
- [x] _Shortcut semantics_ para operadores ``and`` y ``or``.
- [x] Comparadores ( ``<``, ``=`` ).
- [x] Condicionales (``if`` sentences).

El listado refleja tanto los objetivos básicos como los objetivos extra de esta entrega. A continuación se detalla la implementación de cada una de las especificaciones nombradas.

## Implementación

### ``let`` - bindings
La expresión ``let`` posee la siguiente sintaxis:
```Scheme
(let (id val) body)
```
En concreto, permite asignar el valor de la expresión ``val`` a una id, para luego operar con ella en el cuerpo (``body``) de la expresión misma. 

Para la implementación de ``let``, primero se compila el valor de ``val``, quedando en el registro de retorno. Posteriormente, se guarda en el stack con un offset _i_ indicado por el ambiente del compilador:
```
mov [RSP - 8*i], RAX
```
Luego se asocia dicho valor a la ``id``, y se procede a compilar el ``body`` de la expresión con un ambiente extendido con el valor de la ``id``.

De este modo, cuando se necesite obtener el valor de la variable, se busca en __RSP__, utilizando el índice _i_ al que está asociado en el ambiente:
```
mov RAX, [RSP - 8*i]
```

### Valores booleanos
Ahora el lenguaje soporta dos tipos de valores: enteros y booleanos. Puesto que ambos tipos de dato están representados con un entero de 64 bits, se ha separado el bit menos significativo para ser utilizado a modo de marcador; un entero tiene un 0, mientras que un booleano posee un 1.

En la sintaxis del lenguaje, el valor verdadero es `true` y el falso, `false`. Por contraparte, la codificación binaria de estos valores es `0b1000...0001` y `0b0000...0001`, respectivamente. 

### Aritmética de enteros

### Aritmética de booleanos
La sintaxis del operador `not` es `(not p)`, donde p es algún valor booleano. En assembler un not es un `XOR` entre el valor de `p` y `0b1000...0000`.

La sintaxis para `and` y `or` es `(and p q)` y `(or p q)`, respectivamente. Estos operadores poseen atajos de evaluación, lo que significa que el segundo operando sólo se evalúa si el primero no es suficiente para determinar el resultado de la operación. Por esto, para la compilación a assembler no basta usar las instrucciones `AND` y `OR`. Para lograr la semántica deseada, es necesario usar saltos condicionales, de modo que la compilación de `and` resulta en el siguiente patrón:
```
    [...]           # calcular el valor del primer operando: rax <- p
    mov r11, 0x1    # r11 <- false
    cmp rax, r11    # p == false
    je  and_end      
    [...]           # calcular el valor del segundo operando: rax <- q
    mov r11, 0x1    # r11 <- false
    cmp rax, r11    # q == false
    je and_end
    mov rax, 0x8... # rax <- true
and_end:
    [...]           # el resultado queda en rax
```
Esta implementación fue tomada de C, donde las operaciones `&&` y `||` poseen la misma semántica y compilan a algo similar (usando `gcc`).

### Comparadores: <, =
