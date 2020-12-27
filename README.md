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
Tanto funciones definidas fuera del programa principal con `def`, como las funciones lambdas fueron implementadas mediante clausuras, por lo tanto, se tratará a ambas por igual desde este punto, con la única salvedad de que las funciones previamente definidas no requieren añadir variables libres a su scope, y que poseen un nombre definido, lo que simplifica su referencia al aplicar recursión.

Para la implementación de clausuras, se establece una estructura de datos en heap de la siguiente forma:

N° Params | Label | N° Free Ids | Free Id #1 | ... | Free Id #n
-- | -- | -- | -- | -- | --

Esto permite conocer el número de parámetros que recibe la función, un puntero a su codigo fuente mediante su label en *assembly*, el número de variables libres a guardar, y finalmente los valores de tales identificadores libres.

La creación de la clausura de una función se presenta casi en su totalidad en `compile.ml: compile_closure`, donde se ve que el proceso es similar a lo siguiente:
```
mov   [R15], <N° Params>
mov   [R15 + 8], <Label>
mov   [R15 + 16], <N° Free Ids>
mov   [R15 + 24], <Free Id #1>          ;; Solo si aplica
...
mov   [R15 + (16+8k)], <Free Id #k>     ;; Solo si aplica
```
Tras crear la clausura, esta se guarda en un registro a conveniencia y se le añade un tag identificador para clausuras `111`. Podemos recordar los tags en la siguiente tabla:

| Tipo de dato | Tag |
| ------------ | --- |
| Entero (int) | `__0` |
| Booleano     | `001` |
| Tupla        | `011` |
| Record       | `101` |
| **Clausura** | `111` |

Adicionalmente, se hace un análogo a los métodos Python, donde se pasa una referencia a *self* como primer argumento implícito. En este caso, el primer argumento de cada llamada a función (sea λ o definida) será la misma clausura, permitiendo una recursión sencilla en el caso de las funciones definidas con `def`, ya que el nombre de la función corresponde a su label en *assembly*. En contraparte, funciones lambdas no tienen esta facilidad, ya que el interior de una función no sabe cómo fue llamada por fuera.

#### Free Ids
Como se mencionó previamente, una función anónima podría necesitar capturar los identificadores asociados a algún valor al momento de su definición. Se vio que estas son guardadas en la clausura misma, por lo que al momento de aplicación es posible acceder a ellas mediante **[RDI + (2+8*i*)]**. Sin embargo esto complica un poco el *lookup* de ids, ya que estas podrían estar en Stack (como había sido hasta ahora), o en la clausura presente como primer argumento recibid. Sin mencionar que en el código principal RDI no contiene ninguna clausura.

Para normalizar la búsqueda, antes de compilar el body de una función, se hace un paso de **RDI** al *Stack* de todos los valores de identificadores libres capturados, facilitando la búsqueda y acceso durante la ejecución del programa. Por ejemplo, la función `add5`, posee como variable libre a `n`:
```
(let (n 5)
    (let (add5 (λ (x) (+ x n)))
        (add5 3)))
```
Por lo que posterior al llamado de `add5`, en su stack frame ocurre lo siguiente:
```
push RBP                      // Calling Convention
mov  RBP, RSP                 // Calling Convention        
sub  RSP, 0x10                // Space in Stack (16 bytes)
sub  RDI, 0x7                 // Untag the Closure in RDI
mov  R11, qword[RDI + 24]     // Get n from closure
mov  qword[RBP - 8], R11      // Save n in Stack
add  RDI, 0x7                 // Tag the Closure in RDI
```
Se es consciente de que la decisión anterior puede hacer que los programas hagan un uso mas intensivo de la pila de ejecución, pero pareció más acertado.

## Tests
La carpeta `tests` contiene tests variados para el compilador. En específico, para esta entrega se desarrollan casos de prueba en:
- `tests/functions/first_class`: Implementación de First Class

Los tests requeridos:
*a)* Disponible en `fundefs_05.test`
```
(def (mapPair f p) (tup (f (get p 0)) (f (get p 1))))

(let (add3 (λ (x) (+ 3 x)))
  (mapPair add3 (tup 39 2)))
```
*b)* Disponible en `lambda_05.test`
```
(def (fib n) 
    (if (= n 0) 
        0
        (if (= n 1)
            1
            (+ (fib (- n 1)) (fib (- n 2))))))

(def (pair_fun f g p) 
    (tup (f (get p 0)) 
         (g (get p 1))))
    
(pair_fun (λ (x) (+ 3 x))
          (λ (y) (pair_fun fib (λ (z) (* z y)) (tup y y)))
          (tup 39 2))
```
