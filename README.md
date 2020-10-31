# Mini-Compiler :camel:
__[ CC5116 ] - Diseño e Implementación de Compiladores__.

Sergio Morales & Bryan Ortiz.

## Entrega 2
Para la entrega 2, se esperaba implementar las siguientes especificaciones básicas:

- [x] [Gestión de errores de tipos](#gestión-de-errores-de-tipo-type-checking)
- [x] [Añadir funciones externas (FFI)](#funciones-externas)
- [x] [Compilar funciones de primer orden](#funciones-propias)

Mientres que dentro de los objetivos extra, existían 2 opciones, donde se escogió la segunda:
- [ ] Gestión de errores de recursos
- [x] [Convención de llamada a funciones x86-64](#foreign-interfaces)

 A continuación se detalla la implementación de cada una de las especificaciones nombradas.

## Implementación

### Gestión de errores de tipo (_type-checking_) 

Se implementa una gestión dinamica de errores de tipo en tiempo de ejecución, lo cual impediría la ejecución de sentencias como ``(+ 1 false)`` o ``(not 3)``. 

Para realizar esto, se añadió un _type-checking_ para validar el tipo de valor según corresponda. Como es de esperarse, esta validación varía segun el tipo de dato.

#### **Número**
Para hacer una _checking_ de que el tipo sea **numérico**, se implementa a nivel _assembly_ lo siguiente:
``` 
  mov  error_code_register, not_a_number
  mov  tested_register, RAX
  test tested_register, 0x1
  jnz  error_handler
```
Lo que hace esto es fijar como posible código de error ``not_a_number`` en el registro usado para guardar los códigos de errores (en práctica **RBX**). Luego, se mueve el registro de retorno a el registro que se utilizará para guardar la variable que podría ser errónea ``tested_register`` (en concreto **RCX**).

Finalmente, se hace una instrucción ``test``, la cual realiza un **AND** con la máscar de bits que representa el bit de tipo. Como los números tienen asociado el bit de tipo 0, esta operación debería producir 0 como valor. Por lo tanto, si es distinto a 0, significa que se trata de un booleano, y se hace un salto al ``error_handler``.

#### **Booleano**
Por otro lado, para el _type-checking_ de valores **booleanos**, se emplea lo siguiente:
``` 
  mov  error_code_register, not_a_boolean
  mov  tested_register, RAX
  test tested_register, 0x1
  jz  error_handler
```
El proceso es similar a la verificación numérica, con una ligera diferencia en la condición de salto. Dado que los booleanos poseen 1 en el bit de tipo, la operación ``test`` debería generar un valor distinto a 0 (en particular, 1).

Por lo tanto, si el valor obtenido es 0, significa que se trata de un número encubierto, lo cual puede lograr que la operación que requería un tipo booleano falle.

#### **Manejo de Errores**
Para manejar los errores, se utiliza el label ``error_handler``, que contiene el siguiente código _assembly_:
```
error_handler:
  mov  RSI, RCX
  mov  RDI, RBX
  call error
```
En simple, prepara los argumentos para realizar una llamada a la función C **error**.

#### **Función Error (en C)**
Los errores son derivados a esta función, que en concreto es:
```C
void error(int errCode, val v) {
  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "Expected number, but got %s\n", value_to_str(v));
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Expected boolean, but got %s\n", value_to_str(v));
  } else {
    fprintf(stderr, "Unknown error code: %d", errCode);
  }
  exit(errCode);
}
```
Según el código de error, imprime por salida estándar el error de tipo asociado.

#### **Expresiones con _type-checking_**
Se implementó una gestión de errores de tipo para cada una de las siguiente operaciones disponibles del lenguaje:

1. **UnOps**: Para cada una de las operaciones unarias (_ie:_ ``add1``, ``sub1`` y ``not``), se implementa un verificador de tipo antes de aplicar tal operación sobre su argumento.

2. **BinOps**: Para las operaciones binarias numéricas (_ie:_ ``+`` ``-`` ``*`` ``/``), se implementa un verificador de tipo numérico después de compilar cada uno de sus argumentos. Para el caso de ``<`` se realiza el mismo proceso, mientras que ``=`` no posee verificador de tipos, ya que booleanos y enteros son comparables mediante la igualdad (claramente son distintos). Por otro lado, los operadores binarios booleanos, implementan una verificación de tipo booleano después de cada argumento evaluado.

3. **If**: se verifica que la condición sea de tipo booleano, justo después de compilarla.

### Funciones Externas
Se añaden llamados a funciones externas en C. Para ello fue necesario implementar la convención de llamada ``x64`` (la cual también es aplicada a funciones internas).

Para aplicar una función externa, primero esta requiere estar previamente definida en el archivo ``rtsys.c``. Luego, debe declararse como:
```
extern function
```
en el encabezado del código _assembly_ del programa. Finalmente, el caller debe respetar la convención de llamado, pasando los argumentos en los registros correspondientes o por stack.

Para implementar la _calling convention_, de creó una lista con los 6 registros de paso de parámetros: **RDI, RSI, RDX, RCX, R8, R9**.

Primero, se hace un push de los registros a utilizar para el paso de parámetros, esto mediante la función ``push_regs``. Luego, la función ``prepare_call`` inserta los resultados de compilar cada uno de los argumentos en los registros correspondientes (o en stack de ser necesario). Cabe destacar que como cada uno de los argumentos se guarda en orden inverso a como se declaran, su compilación también se realiza en orden inverso. Es decir:
```
(foo (print 1) (print 2))
```
Genera por salida estándar 
```
> 2
> 1
```
Es importante tener en cuenta la aridad de la función definida, ya que si no se entrega la cantidad de argumentos requeridos, habrá un error en tiempo de compilación. 

Con esto ya está todo listo para llamar a la funcion con ``call``. Sin embargo después de ello, es necesario restaurar el **RSP**, por lo que se le suma el espacio que fue desplazado (de haber sido necesario) al hacer ``push`` de los argumentos. Finalmente se restauran los registros utilizados durante el paso de parámetros en la llamada.

### Funciones Propias
Una función se define con una declaración de la forma
```
(def (<function_name> <parameters>*) <body_expr>)
```
al inicio de un programa. Por ejemplo, la función identidad se define con:
```
(def (id x) x)
```
La compilación de un llamado a función de este tipo es idéntico al descrito anteriormente en el caso de las funciones de C. La única diferencia es que ahora la aridad de las funciones ya no está "hard-coded", por lo que es necesario guardar en algún lugar esta información. Para ello, se redefine el ambiente de compilación del siguiente modo:
```
type let_env = (string * int) list
type fun_env = (string * int) list
type env = let_env * fun_env
```
donde ``let_env`` almacena el offset con respecto a la base del stack (posición en memoria) de los identificadores ``let``, y ``fun_env`` almacena las aridades de las funciones declaradas al inicio de un programa. Esto significa que una función y una variable pueden tener el mismo nombre sin causar conflicto.
La compilación de la definición de una función, por otro lado, se reduce a compilar el cuerpo (una expresión) en un ambiente donde los parámetros se encuentren ligados a su identificador correspondiente, y añadir al inicio y al final secciones de acuerdo con la convención de llamados x64 (guardar y restaurar RBP, mover RSP, etc), preceder todo esto por una etiqueta con el nombre de la función. 
El uso de la convención x64 significa que ahora las variables pueden encontrarse en registros, por lo que es necesario modificar nuevamente la definición de un ``let_env``.
```
type memloc =
  | MReg of reg
  | StackOffset of int

type let_env = (string * memloc) list
```
Finalmente, el ``fun_env`` en que se compila el cuerpo de un programa puede ser determinado antes de compilar el cuerpo de las funciones, por lo que al hacerlo en este ambiente se hace posible definir funciones recursivas fácilmente. Así, el siguiente programa con funciones mutuamente recursivas es válido y se ejecuta según lo esperado:
```
(def (pin n) 
  (if (= n 0)
    0
    (pon (- n 1))))

(def (pon n) 
  (if (= n 0)
    0
    (pin (- n 1))))

(pin 10)
  
```
### Interfaz de Funciones Foráneas
Se implementa un mecanismo genérico para el llamado de funciones C. Para definir una función externa, será necesario definirla previamente de la siguiente forma:
```
(defsys <function_name> <arg_types>* -> <return_type>)
```
Por ejemplo, si se quiere definir la función ``min``, sera necesario definirla como:
```
(defsys min int int -> int)
```
Declarando que la función recibe 2 enteros y retorna un entero.

Para realizar la llamada a las funciones, es necesario añadir un ``@sys`` a su nombre, indicando que se trata de una función del sistema. Esto último permite una mejor convivencia entre funciones de C y las funciones propias del lenguaje. Siguiendo con el ejemplo de ``min``, siguiendo esta convención, se debe invocar como:
```
(@sys min 1 3)
```
En general, la forma de invocar funciones de C (debidamente definidas con ``defsys``), es:
```
(@sys <function_name> <arg_1> ... <arg_n>)
```

Este diseño, permite hacer un _checking_ de aridad y de tipos, tanto de los argumentos como de lo retornado, levantando un error en tiempo de ejecución en caso de detectar una inconsistencia con lo declarado.

Puesto que la información necesaria para llamar funciones externas (aridad,tipo de los parámetros, tipo de retorno) es distinta a la necesaria para funciones internas (aridad únicamente), se redefine el ambiente de compilación del siguiente modo:
```
type dtype =
  | IntT
  | BoolT
  | AnyT

type sys_env = (string * dtype list * dtype) list 

type env = let_env * fun_env * sys_env
```
Así, además, el espacio de nombres para funciones externas queda separado de el de funciones propias.

Al adoptar esta interfaz, ya no se podrán llamar primitivas en C como print sin antes definirlas debidamente con su ``defsys``.


## Tests
Los tests fueron expandidos para testear a mayor profundidad las características implementadas dentro del compilador, en particular, dado que en esta entrega se implementa el _checking_ de tipos, se añadieron varios tests que comprueban que estos errores se produscan.

Los tests al pipeline completo se encuentran en archivos ``*.test``, separados en subcarpetas según la funcionalidad que intentan evaluar.

- ``\binOp``: operaciones binarias ``+ - * / < = and or``.
- ``\c_calls``: funciones foráneas de C (como ``print``).
- ``\functions``: funciones propias.
- `if`: condicionales.
- `let`: definición de variables locales.
- `types`: tests respecto a tipos (boolean / int).
- `unOp`: operaciones unarias `add1 sub1 not`.

Para el testing del intérprete y parser, se generan distintas baterías de tests en ``bin\test.ml``.
