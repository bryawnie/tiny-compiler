# Mini-Compiler :camel:
__[ CC5116 ] - Diseño e Implementación de Compiladores__.

Sergio Morales & Bryan Ortiz.

# Entrega 3
Esta entrega contempla algunas mejoras a la versión `v0.2`, donde se proteje el proceso de llamado de funciones (impidiendo un solapamiento de llamadas), y se realiza una refactorización de código con fines de mejorar su comprensión. 

Respecto a las nuevas *features* del lenguaje, los requerimientos básicos para esta entrega son:
- [x] Tuplas.
- [x] `get` en tuplas.
- [x] Mutación de tuplas (`set`).

Mientras que los requerimientos extra:
- [x] Records.
- [x] Pattern-matching de tuplas.

Todas ellos fueron implementados.

## Especificación

### Tuplas
Una tupla es un tipo de dato que consiste en una secuencia de `n` elementos. Una tupla se construye con la operación `(tup e1 ... en)`, donde `e1 ... en` son los elementos de esta. Para operar sobre ella se utiliza `(get n t)` para obtener el `n`-ésimo elemento de la tupla `t` y, análogamente, se utiliza `(set n t v)` para cambiar su valor a `v`. Al igual que las listas y arreglos en muchos lenguajes de programación, los índices comienzan desde 0.

### Record
Los records son estructuras de datos similares a las `struct` de C. Cada tipo de record posee un identificador y cero o más campos con nombre, similares a los elementos de una tupla. Para declarar un nuevo tipo de record se utiliza una expresión de la forma `(record <id> <campo1> ... <campon>)`. Una vez definido un record se pueden utilizar las siguientes funciones:
-  Constructor `<id> : any ... any -> record`. Recibe tantos argumentos como campos tenga el tipo de record y retorna un record de tipo `<id>` con los valores proporcionados.
- `<id>-<campoi> : record -> any`. Retorna el valor del campo `<campoi>` del record proporcionado. El record debe ser de tipo `<id>`. Se crea una de estas funciones por cada campo.
- `<id>? : any -> bool`. Determina si el elemento pasado como argumento es un record del tipo `<id>`.

### Let Upgrade
Resultó conveniente hacer un *upgrade* a las expresiones `let`. En concreto, antes cada expresión sólo podía introducir un nuevo identificador en *scope*, y la sintaxis era:
```
(let (<id> <val>) <body_exp>)
``` 
ahora, un let ofrece la posibilidad de introducir un número indefinido de variables de una sola vez, pasando a tener una sintaxis:
```
(let ((<id_1> <va_1>) (<id_2> <val_2>) ... (<id_n> <val_n>)) <body_exp>)
```

Un ejemplo puede ser la introducción de tres variables `x`, `y`, `z`, con valores 3, 4 y 5:

**Antes**: Estaba la necesidad de introducir cada variable en un `let` distinto.
```
(let (x 3)
  (let (y 4)
    (let (z 5)
      (+ (+ x y) z))))
```
**Ahora**: Basta con una sóla instancia de `let`.
```
(let ((x 3) (y 4) (z 5))
  (+ (+ x y) z))
```
_NOTA:_ A modo de simplificar la introducción de una sola variable, se mantiene retrocompatibilidad con la sintaxis previamente existente.

### Pattern-matching

Un _pattern-matching_ de tuplas, consiste en una expresión que permite descomponerlas en variables que contengan sus elementos. La sintaxis utilizada es:
```
(let ((tup <id_1> <id_2> ... <id_n>) t)
          <body>)
```
donde t es una tupla previamente existente que contiene exactamente `n` valores. Un ejemplo puede ser:
```
(let ((tup a b c) (tup 1 3 5))
          (+ a b))
```
cuyo resultado debiese ser 4.

## Implementación
### Tuplas
La natureleza compuesta y el largo variable de este tipo de dato significan que no cabe en un registro del procesador. Por tanto, para implementarlo es necesario que el programa pueda hacer uso del *heap*. Así, un valor de tipo `Tuple` es en realidad un puntero a un espacio en el *heap*, y es ahí donde se encuentran los valores de la tupla. Para poder saber el largo de esta estructura, los valores siempre están precedidos en el heap por un entero que contiene esa información.

Ahora bien, puesto que este es el tercer tipo de dato añadido al lenguaje, ya no es posible utilizar un único bit para determinar el tipo de un valor. Para solucionar este problema es necesario utilizar más bits. Si se asume que todos los elementos almacenados en el heap son valores de 64 bits, entonces los últimos tres bits de un puntero son siempre iguales para una misma dirección de inicio del heap. Así, basta imponer que el inicio del heap esté a alineado a un múltiplo de 8 para asegurar que los últimos 3 bits de los punteros serán 0, con lo que es posible utilizar esos bits como tag y aún así tener la capacidad de recuperar el puntero original. Con esto, se establecen los siguientes marcadores de tipo:

| Tipo de dato | Tag |
| ------------ | --- |
| Entero (int) | `__0` |
| Booleano     | `001` |
| Tupla        | `011` |

Nótese que cualquier valor que termine en `0` se considera un entero, lo que permite mantener el tamaño de ese tipo de dato en 63 bits.

#### HEAP
Se habló de utilizar el *heap* para guardar los valores de las tuplas, pero no de cómo acceder a esta porción de memoria. Antes de instanciar el programa principal de nuestro lenguaje, se realiza una petición de memoria con `calloc`, la cual es traspasada como parámetro a través del registro **RDI**, que recordemos corresponde al primer argumento según la *calling-convention* x64. Luego, este puntero al inicio del *heap* es guardado en el registro **R15**, utilizado desde ahora como un *heap pointer*.

#### Constructor de tuplas
Para construir una tupla del estilo:
```
(tup <v1> <v2> ... <vn>)
```
Primero se compila el valor de `v1` ... `vn`, moviendo al *stack* los valores obtenidos. Es decir, si por simplicidad asumimos que no hemos guardado ninguna variable local aún, `v1` se encontrará en **[RBP-8]**, `v2` en **[RBP-16]**, y para un *i* arbitrario, `vi` se encontrará en **[RBP-8\*i]**. Una vez compilados todos los valores de la tupla, se procede a moverlos al heap, guardando primero el tamaño de la tupla en **[R15]**, y luego su contenido, donde en general, `vi` queda guardado en **[R15+8\*i]**.

Tras lo anterior, se guarda el puntero al inicio de la tupla en el registro de retorno **RAX**, y se actualiza **R15** para poder ofrecer un espacio libre en el *heap* nuevamente. Para ello se desplaza en *8\*(n + 1)* posiciones, añadiendo 8 adicionales si no queda alineada a una dirección múltiplo de 16.

#### Get

### Mutación de tuplas
...

### Records
En términos de su representación, los records son muy similares a las tuplas. La principal diferencia es el tag: `101` para records en vez de `011` para tuplas. La segunda diferencia es que el entero de 64 bits con que comienza un record en el heap no sólo indica su tamaño, sino además su tipo, de modo que los primeros 32 bits del valor son un tag generado en tiempo de compilación, y los últimos 32 son el tamaño de la estructura.

Las operaciones sobre records, por otra parte, si son distintas. El constructor, las funciones de proyección (`id-campo`) y el _type-checker_ (`id?`) son funciones, no operadores. Es decir, se llaman con `call` e intentar definir otra función con el mismo nombre causa un error de compilación. A diferencia de las funciones definidas por el usuario, estas no se compilan a partir de una expresión, sino que el código _assembler_ se genera directamente, se le añade una etiqueta para poder invocarlo con `call`, y dicha etiqueta se añade al ambiente de compilación del programa junto con su aridad.

#### Constructor
El procedimiento es básicamente idéntico a la creación de una tupla. La única diferencia es el tag.

#### Getter
Muy similar al `get` para tuplas, con la diferencia de que cada función accede a un elemento específico. Por ejemplo, si `field` es el `n`-ésimo campo de un _record_ de nombre `id`, entonces `(id-field t)` es análogo a `(get n t)`. Sin embargo, no se puede usar exactamente el mismo código porque, por un lado, `get` corrobora la validez del índice, que en este caso es innecesario, y por el otro `id-field` necesita, además de verificar que el valor recibido sea un puntero a un record, asegurarse de que los valores en el heap corresponden efectivamente a una estructura de tipo `id`. En consecuencia, en vez de comparar el índice contra el tamaño de la tupla, `id-field` compara el primer elemento del record contra el tag (conocido al momento de compilar la función) y levanta un error de tipo si no coinciden.

#### _typechecker_
El verificador de tipo (`id?`) primero comprueba que el valor recibido sea un puntero a un record, y retorna `false` si no lo es. Si es un puntero válido, entonces lee el primer elemento y lo compara contra el tag asociado a `id`; si son distintos retorna `false` y si son iguales, `true`.

### Pattern-matching de tuplas
...

## Otros cambios
- `print` ahora se incluye por defecto en el ambiente de compilación del lenguaje. Esto significa que ya no es necesario definirla con `defsys` para poder usarla en un programa.
- En los llamados a funciones, el valor de cada argumento se compila y se guarda en *stack* antes de ser movido a los parámetros de la convención de llamada. Esto protege la sobreescritura cuando se realiza una composición de llamados.
- Se crea un generador de `gensym`s.
- Se modulariza la función `compile_expr`, mejorando la legibilidad del código.
- Se estandarizan los errores.
- Se introducen comentarios en el assembly.
- Se añade una conversión de tipos antes y después de un llamado a función externa de C, a fin de asegurar la usabiidad por ambos lados.
- Parser admite diferentes tipos de expresiones `let`.
- Se añade una función `len` para obtener el largo de una tupla de manera sencilla.
