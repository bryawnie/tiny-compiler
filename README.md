# Mini-Compiler :camel:
__[ CC5116 ] - Diseño e Implementación de Compiladores__.

Sergio Morales & Bryan Ortiz.

## Entrega 2
Para la entrega 2, se esperaba implementar las siguientes especificaciones básicas:

- [x] [Gestión de errores de tipos](#gestión-de-errores-de-tipo-type-checking)
- [x] [Añadir funciones externas (FFI)](#funciones-externas)
- [x] [Compilar funciones de primer orden](#TODO)

Mientres que dentro de los objetivos extra, existían 2 opciones, donde se escogió la segunda:
- [ ] [Gestión de errores de recursos]
- [x] [Convención de llamada a funciones x86-64](#TODO)

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



## Tests
Se implementan tests al pipeline completo en archivos ``*.test``. Para el testing del intérprete y parser, se generan distintas baterías de tests en ``bin\test.ml``.

Un ejemplo de test que utiliza todas las features implementadas en esta entrega se encuentra en ``fulltest_e1.test``, el cual prueba el funcionamiento del programa:

```Scheme
(let (x 5)
    (let (y (add1 (sub1 x)))
        (let (x (if (< y 5) (* x 8) (* x 12)))
            (if (and (= x 60) (and (= y 5) (not false)))
                (let (z (sub1 (/ x 4)))
                    (/ (* -4 (- z 19)) 2))
                false))))
```
Que entrega como resultado 10.
