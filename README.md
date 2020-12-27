# Mini-Compiler :camel:
__[ CC5116 ] - Diseño e Implementación de Compiladores__.

Sergio Morales & Bryan Ortiz.

# Entrega 4
En esta entrega se añade una esperada *feature* para el lenguaje, **funciones de primera clase**. Recordamos que en las versiones previas, solo se contaba con funciones de primer orden, las cuales son bastante limitadas, dado que deben definirse de forma aparte, y además no podían ser tratadas como valores. En esta nueva versión, se añade soporte a las siguientes *specs básicas* esperadas:
- [x] Funciones como valores de primera clase.
- [x] Lambdas y clausuras.

La característica adicional
- [ ] Recursión.

No fue implementada (yet).

## Especificación
### Funciones como Valores
Se realiza un *upgrade* a las funciones de primer orden previamente implementadas por el lenguaje. En este nuevo escenario, estas pueden ser tratadas como valores, permitiendo su paso a través de llamada de funciones. Un ejemplo puede ser el siguiente:
```
(def (applyToFive it)
  (it 5))

(def (incr x)
  (+ x 1))

(applyToFive incr)
```
Se observa cómo `applyToFive` recibe como argumento una función `it`, la cual es aplicada al valor 5. Es decir, ahora una función pasa a ser una `expr` como cualquier otra.

### Lambdas (λ)
Se implementan funciones lambdas (o anónimas), las cuales pueden definirse dentro de una sentencia `let`. Estas funciones no poseen nombre alguno, y su existencia permite funciones de orden superior como `addn`, la cual recibe un argumento `n` y retorna una función que suma n a su propio argumento. Se presenta un ejemplo:
```
(let (addn (λ (n) 
            (λ (x) (+ x n))))
    (let (add5 (addn 5))
        (add5 6)))
```
El programa anterior debería devolver 11, pues `add5` es una función que suma 5 a su argumento.

Se observa a través del ejemplo que la sintaxis concreta para definir funciones anónimas es:
```
(λ (<ids> ...) <body>)
```

### Clausuras
Al momento de definir funciones, en especial las lambdas, se hace necesario capturar el ambiente de definición para conservar *scope* léxico. Esto permite poder acceder a las ids definidas fuera de la función, las cuales podrían ser detectadas como identificadoes libres si no se resguardan adecuadamente. Por ejemplo:
```
(let (n 5)
    (let (addn (λ (x) (+ x n)))
        (addn 3)))
```
El programa anterior debería entregar como resultado 8. Sin embargo, si el acceso a `n` dentro de la función se pierde, generaría un error. Por tanto, es necesario encapsular esto en clausuras. En particular, las funciones definidas con `def`, no requieren guardar ninguna variable libre ya que al momento de su definición, aun no se introduce ninguna variable en *scope*. Sin embargo, se opta por representarlas mediante clausuras igualmente, para mayor armonía entre funciones.

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

Todo esto es realizado por la función ``compile_tuple``, la cual releva a `compile_expr` en el proceso de compilación de tuplas. Esta también se apoya de `comp_tuple_elems` responsable de compilar los valores de las tuplas y dejarlas guardadas en *stack*. Además de `assign_tuple_values`, función encargada de asignar los valores desde *stack* a la tupla generada.

#### Get
Los *getters* de las tuplas poseen la siguiente estructura:
```
(get <tuple> <position>)
```
Donde `<tuple>` corresponde a una expresión que represente una tupla y `<position>` un índice válido dentro de la tupla. Antes de realizar el acceso, existen verificaciones que impiden un uso malicioso del programa. La posición debe ser un número válido, positivo y menor al largo de la tupla. Por otro lado, la primera expresión debe ser una tupla. Estas verificaciones son posibles gracias al sistema de `tagging` mencionado previamente, y tal tarea se encuentra delegada a las funciones `type_checking` y `check_index`.

Una vez que se verificó la coherencia de la expresión, se procede a eliminar el tag de la tupla para poder acceder a sus valores. Se suma 2 al índice considerando la representación entera y que los index comienzan de 0 externamente y desde 1 internamente, luego se hace un *shift* aritmético a la derecha del index, a fin de obtener el índice real *i* a consultar.

Finalmente, considerando que la tupla está guardada en **RAX**, se obtiene su *i*-ésimo valor, ubicado en **[RAX + 8\**i*]**. Sin embargo, *i* se encuentra en el registro auxiliar **R11**, por lo tanto, como los desfases se encuentran implementados sólo con constantes, se realiza lo siguiente:
```
imul  R11, 8
add   RAX, R11
mov   RAX, [RAX] 
```
Quedando el resultado en **RAX**.

La función encargadas de la compilación de *getters* es ``compile_get``.

### Mutación de tuplas
La mutación se aplica de una forma similar a los *getters*:
```
(set <tuple> <position> <value>)
```
La función `compile_set` se encarga de compilar `<value>`, guardarlo en *stack*, compilar `<position>` y `<tuple>` (realizando las mismas verificaciones de tipo que con `get`) y finalmente asignar el valor en la tupla. Para esto, nuevamente es necesario eliminar el tag, aplicar el desfase deseado sumando el índice, transferir el valor, y luego eliminar el desfase y aplicar el tagging nuevamente:
```
sub   RAX, 3          ;; elimina el tagging
imul  R11, 8          ;; 8*i
add   RAX, R11        ;; Aplica el desfase a <position>
mov   [RAX], <value>  ;; Guarda el valor
sub   RAX, R11        ;; Vuelve al inicio de la tupla
add   RAX, 3          ;; restaura el tagging
```

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
Considerando que ahora se cuenta con un `let` múltiple y con `getters` para tuplas, es posible introducir el *pattern-matching*:
```
(let ((tup <id1> <id2> ... <idn>) <tuple>)
          <body>)
```
como un azúcar sintáctico de:
```
(let ((<id1> (get <tuple> 0))
      (<id2> (get <tuple> 1))
      ... 
      (<idn> (get <tuple> n-1)))
          <body>)
```
el cual puede ser resuelto en el parser.

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

## Tests
La carpeta `tests` contiene tests variados para el compilador. En específico, para esta entrega se desarrollan casos de prueba en:
- `tests/functions`: Arreglo en convención de llamada y funciones en C sobre tuplas.
- `tests/tuples`: Tuplas, getters y setters.
- `tests/records`: Records.
