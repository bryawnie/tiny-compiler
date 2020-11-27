# Mini-Compiler :camel:
__[ CC5116 ] - Diseño e Implementación de Compiladores__.

Sergio Morales & Bryan Ortiz.

# Entrega 3
Los requerimientos básicos para esta entrega son:
- Tuplas
- Mutación de tuplas
Y los requerimientos extra:
- Records
- Pattern-matching de tuplas
Todas estos requerimientos fueron implementados.

## Especificación

### Tuplas
Una tupla es un tipo de dato que consiste en una secuencia de `n` elementos. Una tupla se construye con la operación `(tup e1 ... en)`, donde `e1 ... en` son los elementos de esta. Para operar sobre ella se utiliza `(get n t)` para obtener el `n`-ésimo elemento de la tupla `t` y, análogamente, se utiliza `(set n t v)` para cambiar su valor a `v`.

### Record
Los records son estructuras de datos similares a las `struct` de C. Cada tipo de record posee un identificador y cero o más campos con nombre, similares a los elementos de una tupla. Para declarar un nuevo tipo de record se utiliza una expresión de la forma `(record <id> <campo1> ... <campon>)`. Una vez definido un record se pueden utilizar las siguientes funciones:
-  Constructor `<id> : any ... any -> record`. Recibe tantos argumentos como campos tenga el tipo de record y retorna un record de tipo `<id>` con los valores proporcionados.
- `<id>-<campoi> : record -> any`. Retorna el valor del campo `<campoi>` del record proporcionado. El record debe ser de tipo `<id>`. Se crea una de estas funciones por cada campo.
- `<id>? : any -> bool`. Determina si el elemento pasado como argumento es un record del tipo `<id>`.

### Pattern-matching
...

## Implementación
### Tuplas
La natureleza compuesta y el largo variable de este tipo de dato significan que no cabe en un registro del procesador. Por tanto, para implementarlo es necesario que el programa pueda hacer uso del heap. Así, un valor de tipo `tuple` es un realidad un puntero a un espacio en el heap, y es ahí donde se encuentran los valores de la tupla. Para poder saber el largo de la tupla, los valores siempre están precedidos en el heap por un entero que contiene esa información.

Ahora bien, puesto que este es el tercer tipo de dato añadido al lenguaje, ya no es posible utilizar un único bit para determinar el tipo de un valor. Para solucionar este problema es necesario utilizar más bits. Si se asume que todos los elementos almacenados en el heap son valores de 64 bits, entonces los últimos tres bits de un puntero son siempre iguales para una misma dirección de inicio del heap. Así, basta imponer que el inicio del heap esté a alineado a un múltiplo de 8 para asegurar que los últimos 3 bits de los punteros serán 0, con lo que es posible utilizar esos bits como tag ý aún así tener la capacidad de recuperar el puntero original. Con esto, se establecen los siguientes marcadores de tipo:

| Tipo de dato | Tag |
| ------------ | --- |
| Entero (int) | `xx0` |
| Booleano     | `001` |
| Tupla        | `011` |

Nótese que cualquier valor que termine en `0` se considera un entero, lo que permite mantener el tamaño de ese tipo de dato en 63 bits.

#### Constructor de tuplas
```
```
#### Get
#### Set

### Mutación de tuplas
...
### Records

### Pattern-matching de tuplas
...

## Otros cambios
- `print` ahora se incluye por defecto en el ambiente de compilación del lenguaje. Esto significa que ya no es necesario definirla con `defsys` para poder usarla en un programa.
