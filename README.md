# Mini-Compiler
__[ CC5116 ] - Diseño e Implementación de Compiladores__.

Sergio Morales & Bryan Ortiz.

## Entrega 1
Para la entrega 1, se implementan las siguientes especificaciones:
- [x] [``let``-bindings.](#let---bindings)
- [x] [Valores Booleanos ( ``true``, ``false`` ).](#valores-booleanos)
- [x] [Aritmética de Enteros ( ``+ - * /`` ).](#aritmética-de-enteros)
- [x] [Aritmética de Booleanos ( ``and``, ``or``, ``not`` ).](#aritmética-de-booleanos)
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

### Valores Booleanos
Ahora el lenguaje soporta dos tipos de valores: enteros y booleanos. Puesto que ambos tipos de dato están representados con un entero de 64 bits, se ha separado el bit menos significativo para ser utilizado a modo de marcador; un entero tiene un 0, mientras que un booleano posee un 1.

En la sintaxis del lenguaje, el valor verdadero es `true` y el falso, `false`. Por contraparte, la codificación binaria de estos valores es `0b1000...0001` y `0b0000...0001`, respectivamente. 

### Aritmética de Enteros
Previo a introducir los operadores aritméticos, es necesario recordar que la representación de los enteros en el compilador es de 63 bits, por lo tanto, cada número se ve representado por su doble (1 → 2) a nivel implementación.

Dentro de las operaciones aritméticas entre valores enteros, el compilador permite operadores __unarios__ y __binarios__.

__Operadores Unarios__: Existen 2 operadores unarios, ``add1`` y ``sub1``. El primero incrementa el valor de una expresión numérica en 1, mientras que el segundo la decrementa en 1. La implementación consiste en compilar la expresión inicial y luego aplicar:
```
add     RAX, 1
```
o bien:
```
sub     RAX, 1
```
según corresponda.

__Operadores Binarios__: En este grupo, se encuentran la adición (``+``), substracción (``-``), producto (``*``) y división (``/``). La implementación de cada uno de ellos es bastante similar.

Primero, dado que tenemos una operación del estilo:
```Scheme
(op exprL exprR)
```
La idea general es compilar ``exprR``, guardar su valor en un registro temporal ``temp`` y luego compilar ``exprL``. De este modo, como el valor de ``exprL`` sigue en __RAX__ es posible generar el _assembly_ de la operación:
```
asm_op     RAX, temp
```
La función ``compile_binop_preamble`` dentro de __compile.ml__ se encarga de compilar el codigo común para las 4 operaciones.

Con lo anterior, se tiene de forma directa la adición y substracción. Para el producto, es necesario recordar que la representación de un entero _n_ es _2n_. Por lo que al multiplicar _2n*2k_ el resultado es _4nk_, cuando lo que queremos obtener es _2nk_. Por lo tanto, se añade un _shift_ a la derecha para dividir por 2, quedando el producto como:
```
imul     RAX, temp
sar      RAX, 1
```
Para la división existe otra sutileza; el operador de _assembly_ ``idiv`` recibe sólo un argumento y lo opera con __RAX__. Es decir:
```
idiv     temp
```
debería calcular la división. Sin embargo, aún queda un detalle; la división en _assembly_ divide ``RDX-RAX/temp``. Al no haber utilizado el registro __RDX__ podría tener un contenido indeseado, por lo tanto es necesario limpiarlo antes de operar. Además, nuevamente haciendo alusión a la representación, _2n/2k=n/k_, cuando lo buscado es obtener _2(n/k)_, haciendo necesario un _shift_ a la izquierda. En resumen, para la división el asm generado corresponde a:
```
mov     RDX, 0
idiv    temp
sal     RAX, 1
```

__Operadores Binarios__
### Aritmética de Booleanos
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
