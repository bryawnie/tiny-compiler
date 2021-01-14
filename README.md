# Mini-Compiler :camel:
__[ CC5116 ] - Diseño e Implementación de Compiladores__.

Sergio Morales & Bryan Ortiz.

# Entrega 5
En esta entrega se añade un Garbage Collector para el lenguaje, el cual se basa en el [Algoritmo de Cheney](https://en.wikipedia.org/wiki/Cheney%27s_algorithm) (C.J. Cheney). El método consiste en dividir el espacio del *heap* en dos mitades iguales, de las cuales, sólo una se encuentra en uso a la vez.

La recolección de basura se realiza copiando los objetos "vivos" de un espacio (`from-space`) al otro (`to-space`). Este último se convierte en el nuevo espacio de objetos.

## Especificación
Al ejecutar un programa, se le puede pasar por parámetro el tamaño que dispondrá el *heap*, en número de "slots" de 8 bytes. Es decir, si se fija:
```
HEAP_SIZE=2
```
El tamaño del *heap* será de 2 "slots" de 8 bytes, equivalente a 16 bytes. Este tamaño corresponde tanto al del `from-space` como el de el `to-space`, por lo que finalmente, el pedido de memoria al sistema será de 32 bytes.

Adicionalmente, se puede fijar si se desea o no utilizar el Garbage Collector añadido, con la opción:
```
USE_GC=0 ;; Si no se desea utilizar el GC.
USE_GC=1 ;; Si se desea utilizar.
```

Por defecto, los valores son `HEAP_SIZE=16` y `USE_GC=1`.

El razonamiento es sencillo, se inicia el programa guardando las nuevas instancias de objeto (ya sea *tupla*, *clausura* o *record*) en el `from-space`. Una vez que al necesitar alocar un nuevo elemento, la memoria en `from-space` sea insuficiente, se realiza una copia al `to-space` de todos aquellos elementos que sean referenciados desde el frame actual del `stack`, y aquellos referenciados por estos. Finalmente, se vacía el `from-space` y se hace un `swap` entre los espacios, a modo de que los objetos vuelvan a estar en el `from-space`.

## Implementación

### Main
Dentro de la función main del *runtime system*, se consideran las siguientes constantes:
```C
  HEAP_MID    = HEAP_START + HEAP_SIZE;     // Middle of HEAP
  HEAP_END    = HEAP_START + HEAP_SIZE*2;   // End of HEAP
  FROM_SPACE  = HEAP_START;                 // Initial From Space
  TO_SPACE    = HEAP_MID;                   // Initial To Space
```
donde `HEAP_START` es la dirección de inicio del *heap*. Con esto se puede controlar los espacios existentes en el heap, donde (inicialmente) se dividirán como:
```
              HEAP MEMORY
    --------------------------------
    |   FROM SPACE  |   TO SPACE   |
    --------------------------------
    ↑               ↑              ↑
HEAP START      HEAP MID       HEAP END
```

### Cheney's Algorithm
Sigiendo el algoritmo de Cheney, se utilizan las siguientes funciones:

#### Try_GC
Esta función cumple la tarea de gestionar los usos de memoria en el *heap*, verificando si el espacio existente es suficiente para alocar *n* elementos. De no ser así, dispara el llamado a la función `collect`. Su implementación dentro del código C es la siguiente:
```C
val* try_gc(val* alloc_ptr, val words_needed, val* cur_frame, val* cur_sp) {
  if (USE_GC==1 && alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) 
    alloc_ptr = collect(cur_frame, cur_sp);
  if (alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE)
    exit(-1);
  return alloc_ptr;
}
```
Si una vez se realiza el proceso de `collect`, la memoria sigue siendo insuficiente, se levanta un error, terminando la ejecución del programa al instante.

#### Collect
```C
val* collect(val* cur_frame, val* cur_sp);
```
Esta función se encarga de filtrar los elementos vivos en `from-space` al `to-space`, intercambiando estos al final de su ejecución. 

Para ello, primero declara los punteros:
```
  ALLOC_PTR = TO_SPACE; // TO_SPACE IS EMPTY
  SCAN_PTR  = TO_SPACE; // TO_SPACE IS EMPTY
```
donde `ALLOC_PTR`, corresponde al puntero que indica la dirección donde se guardará el nuevo objeto, y `SCAN_PTR`, el puntero que recorrerá el `to-space` posteriormente buscando referencias que aún estén en el `from-space`.

El procedimiento continúa realizando lo siguiente:
```C
  // for root in stack
  for (val* cur_word = cur_sp; cur_word < cur_frame; cur_word++) {
    val v = (val) *cur_word;    // The value in stack
    if (is_heap_ptr(v))         // If pointer to heap
      *cur_word = copy(v);      // Creates a copy in to-space
  }
```
Se analizan todos los elementos que haya en *stack* que referencien a algún objeto vivo en *heap*. Cuando encuentra uno, crea una copia en el *to-space* y actualiza en *stack* una referencia a la nueva instancia.

Una vez han sido analizados todos los elementos en *stack*, queda revisar dentro de los elementos copiados si es que alguno referencia a otro que aún reside dentro del `from-space`:
```C
  // Scanning objects in heap
  while (SCAN_PTR < ALLOC_PTR){
    val v = *SCAN_PTR;
    if (is_heap_ptr(v)) {            // If pointer to heap
      ptr = (val*) v;
      if (!is_forwadding_addr(v))    // If pointer to from-space
        *SCAN_PTR = copy(v);         // Creates a copy in to-space
    }
    SCAN_PTR++;
  }
```
Si *v* es la dirección de algo en `from-space`, se procede a obtener una copia que ahora exista dentro del `to-space`.

Habiendo realizado lo anterior, el contenido de `from-space` deja de ser útil, por lo que se le aplica una limpieza:
```C
val * tmp = FROM_SPACE;
  val * END = (val) HEAP_MID > (val) FROM_SPACE ? HEAP_MID : HEAP_END;
  while (tmp < END) {
    *tmp++ = 0;
  }
```

Finalmente, se realiza un `swap(FROM_SPACE,TO_SPACE)` a modo de que el nuevo `from-space` sea aquel que contiene los elementos vivos, retornando el `ALLOC_PTR`. Este es el puntero a la próxima ubicación en *heap* disponible.

#### Copy
Algo que quedó pendiente de `collect` es la copia de objetos. Para ello se implementa:
```C
val copy(val v){
  val* origin_address = v & ~TAG_BITMASK;
  if (!is_forwadding_addr((val*) *origin_address)){
     // ...
     // Does the copy process
     // ...
  }
  return *origin_address;
}
```
En esta función, primero se obtiene la ubicación real donde se encuentra el objeto a copiar. Si el contenido es una dirección al `to-space` (*forwading-address*), significa que el elemento ya fue movido y se entrega tal referencia. De no ser así, se ejecuta el proceso de copiado, el cual varía según el tipo de estructura a copiar:
```C
case TYPE_<structure>:
  addr = copy_<structure>(v);   // Obtains a copy of <structure>
  *origin_address = addr;       // Sets a forwader
  return addr;                  // Returns the <structure> address
```
Donde `<structure>` puede ser *tuple*, *closure* o bien *record*, cada una de ellas con su función de copiado específica (ver en `rtsys.c` para más detalle).

## Tests
La carpeta `tests` contiene tests variados para el compilador. En específico, para esta entrega se desarrollan casos de prueba en:
- `tests/gc`: Garbage Collector

Un ejemplo de prueba es el siguiente código:
```
(let (t (get (tup 8) 0))
    (let (q (tup 2))
        (let (t (tup q))
            (let (x (set q 0 t))
                (tup)))))
```
Sujeto a los parámetros `HEAP_SIZE=6` y `USE_GC=0`.

En el primer let, se guarda `(tup 8)` en el heap:
| 0x0 | 0x1 | 0x2 | 0x3 | 0x4 | 0x5 |   
-- | -- | -- | -- | -- | -- 
| 1 | 8 |   |   |   |   |   

El segundo *let* introduce `(tup 2)`:
| 0x0 | 0x1 | 0x2 | 0x3 | 0x4 | 0x5 |   
-- | -- | -- | -- | -- | -- 
| 1 | 8 | 1 | 2 |   |   |   

El el tercer *let* necesita almacenar `(tup q)`:
| 0x0 | 0x1 | 0x2 | 0x3 | 0x4 | 0x5 |   
-- | -- | -- | -- | -- | -- 
| 1 | 8 | 1 | 2 | 1 | 0x2 |   

El el cuarto let *let* modifica la tupla `q`:
| 0x0 | 0x1 | 0x2 | 0x3 | 0x4 | 0x5 |   
-- | -- | -- | -- | -- | -- 
| 1 | 8 | 1 | 0x4 | 1 | 0x2 |   

Y el final del programa, requiere dejar `(tup)` en el heap, pero este esta lleno, por lo que se invoca al GC, el cual descubre que la tupla `(tup 8)` no es referenciado en *stack*, por lo cual
