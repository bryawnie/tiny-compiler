#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>
#include <sys/resource.h>

#define TRACE 1

/* synonym to ease typing/reading */
typedef uint64_t val;   // any value
typedef int64_t int_v;  // signed integer
typedef uint64_t bool_v;// boolean

/* configuration */
val STACK_SIZE = 0x800000;
val HEAP_SIZE = 16; // = 16;
int USE_GC = 1;

/* externs */
extern void error(int err_code, val val) asm("error");
extern val print(val val) asm("print");
extern val* try_gc(val* alloc_ptr, int words_needed, val* cur_frame, val* cur_sp) asm("try_gc");
extern val our_code_starts_here(val* HEAP) asm("our_code_starts_here");
extern void set_stack_bottom(val* stack_bottom) asm("set_stack_bottom");
/* */


/* TYPE TAGS*/

const val TAG_BITMASK = 0x7;
const val TAG_INT = 0x0;
const val TAG_BOOL = 0x1;
const val TAG_TUPLE = 0x3;
const val TAG_RECORD = 0x5;
const val TAG_CLOSURE = 0x7;

/* Constants */
const val BOOL_BIT = (0x1UL << 63);
const val RECORD_SIZE_BITMASK = ~0UL >> 32;
const val VAL_TRUE = BOOL_BIT | TAG_BOOL;
const val VAL_FALSE = TAG_BOOL;

/* MACROS */
#define VAL_TO_PTR(v) ((val*) (v & ~TAG_BITMASK))
#define GET_TUPLE_SIZE(v) (*VAL_TO_PTR(v)/2)
#define TUPLE_TO_ARRAY(v) (VAL_TO_PTR(v) + 1)
/* the first element of a record contains two numbers: the 32 least
  significant bits represent its size, and the most significant 32 bits
  are a type tag to differentiate amongst different records. */
#define GET_RECORD_SIZE(v) ((*VAL_TO_PTR(v)) & RECORD_SIZE_BITMASK)
#define GET_RECORD_TYPE(v) (*VAL_TO_PTR(v) & ~RECORD_SIZE_BITMASK) >> 32
#define RECORD_TO_ARRAY(v) (VAL_TO_PTR(v) + 1)
#define CLOSURE_ARITY(v) (*VAL_TO_PTR(v) >> 1)
#define CLOSURE_ADDRESS(v) (*(VAL_TO_PTR(v)+1))

/* Format strings */
const char *STR_TRUE = "true";
const char *STR_FALSE = "false";

typedef enum error_code {
  NOERR,
  ERR_NOT_NUMBER,
  ERR_NOT_BOOLEAN,
  ERR_NOT_TUPLE,
  ERR_NEG_INDEX,
  ERR_INDEX_OVERFLOW,
  ERR_NOT_RECORD,
  ERR_RECORD_TYPE,
  ERR_ARITY_MISMATCH,
  ERR_NOT_CLOSURE
} errcode;

typedef enum data_type {
  TYPE_INT,
  TYPE_BOOL,
  TYPE_TUPLE,
  TYPE_RECORD,
  TYPE_CLOSURE
} dtype;

/* determines the data type of a value */
dtype typeofval(val v) {
  
  // fprintf(stderr, "typeofval(defsys raw_print any -> any): v=%ld\n", v);
   
  if (!(v & 1)) {
    // integer values are always even (last bit is 0)
    return TYPE_INT;
  }
  return (dtype) 1 + (v & TAG_BITMASK)/2;
}

/* applies charcount to an array of values */
int arr_charcount(val *a, int size);

/* Counts the maximum amount of characters required to print a value. */
int charcount(val v) {
  
  // fprintf(stderr, "charcount: v=%ld\n", v);

  switch (typeofval(v)) {

    case TYPE_INT:
      return 20;

    case TYPE_BOOL:
      return 5;

    case TYPE_TUPLE:
      return 5 + arr_charcount(TUPLE_TO_ARRAY(v), GET_TUPLE_SIZE(v));

    case TYPE_RECORD:
      return 2 + arr_charcount(RECORD_TO_ARRAY(v), GET_RECORD_SIZE(v));

    default:
      return 40;
  }
}

int arr_charcount(val *a, int size) {
  
  int count = 0;
  for (int i = 0; i < size; i++) {
    count = count + charcount(a[i]) + 1;
  }
  return count;
}


/* applies printval to an array of values */
void arr_printval(FILE *out, val* a, int size, char* prefix, char* suffix);

/* Prints a value by standard output */
void printval(FILE *out, val v) {

  /* fprintf(stderr, "sprintval: str=%p, v=%ld\n", str, v); */
  
  switch (typeofval(v)) {

    case TYPE_INT:
      fprintf(out, "%ld", ((int_v) v) >> 1);
      break;

    case TYPE_BOOL:
      switch (v) {
        case VAL_TRUE:
          fprintf(out, "%s", STR_TRUE);
          break;
        case VAL_FALSE:
          fprintf(out, "%s", STR_FALSE);
          break;
        default:
          fprintf(out, "Unknown value: %#018lx", v);
          break;
      }
      break;

    case TYPE_TUPLE:
      if (GET_TUPLE_SIZE(v) == 0)
        fprintf(out, "(tup)\n");
      arr_printval(out, TUPLE_TO_ARRAY(v), GET_TUPLE_SIZE(v), "(tup ", ")");
      break;

    case TYPE_RECORD:
      if (GET_RECORD_SIZE(v) == 0)
        fprintf(out, "{}\n");
      arr_printval(out, RECORD_TO_ARRAY(v), GET_RECORD_SIZE(v), "{", "}");
      break;

    case TYPE_CLOSURE:
      fprintf(out, "<Function at %#.16lx with arity %ld>",
        CLOSURE_ADDRESS(v), CLOSURE_ARITY(v));
      break;

    default:
      fprintf(out, "Unknown value: %#018lx", v);
      break;
  }

}

void arr_printval(FILE* out, val *a, int size, char* pre, char* post) {
  /* 
  fprintf(stderr, "arr_sprintval: str=%p, a=%p, size=%d, pre='%s', post='%s'",
    str, a, size, pre, post);
   */
  fprintf(out, "%s", pre);
  for (int i = 0; i < size; i++) {
    printval(out, a[i]);
    if (i + 1 < size) 
      fprintf(out, " ");  
  }
  fprintf(out, "%s", post);
}


/* applies sprintval to an array of values */
int arr_sprintval(char *str, val* a, int size, char* prefix, char* suffix);

/* prints a formatted value to as string */
int sprintval(char *str, val v) {
  
  /* fprintf(stderr, "sprintval: str=%p, v=%ld\n", str, v); */
  char *aux;
  
  switch (typeofval(v)) {

    case TYPE_INT:
      return sprintf(str, "%ld", ((int_v) v) >> 1);

    case TYPE_BOOL:
      switch (v) {
        case VAL_TRUE:
          return sprintf(str, "%s", STR_TRUE);
        break;
        case VAL_FALSE:
          return sprintf(str, "%s", STR_FALSE);
        break;
        default:
          goto unknown;
      }

    case TYPE_TUPLE:
      if (GET_TUPLE_SIZE(v) == 0)
        return sprintf(str, "(tup)");
      return arr_sprintval(str, TUPLE_TO_ARRAY(v), GET_TUPLE_SIZE(v), "(tup ", ")");

    case TYPE_RECORD:
      if (GET_RECORD_SIZE(v) == 0)
        return sprintf(str, "{}");
      return arr_sprintval(str, RECORD_TO_ARRAY(v), GET_RECORD_SIZE(v), "{", "}");

    case TYPE_CLOSURE:
      return sprintf(str, "<Function at %#.16lx with arity %ld>",
        CLOSURE_ADDRESS(v), CLOSURE_ARITY(v));

    default:
    unknown:
      return sprintf(str, "Unknown value: %#018lx", v);
  }
}

int arr_sprintval(char *str, val *a, int size, char* pre, char* post) {
  /* 
  fprintf(stderr, "arr_sprintval: str=%p, a=%p, size=%d, pre='%s', post='%s'",
    str, a, size, pre, post);
   */

  char *s = str;
  s += sprintf(s, "%s", pre);
  for (int i = 0; i < size; i++) {
    s += sprintval(s, a[i]);
    if (i + 1 < size) *s++ = ' ';
      
  }
  s += sprintf(s, "%s", post);
  return s - str;
}

void error(int err, val v) {

  const char *format;

  switch (err){
  case ERR_NOT_NUMBER:
    format = "Expected number, but got ";
    break;
  case ERR_NOT_BOOLEAN:
    format =  "Expected boolean, but got ";
    break;
  case ERR_NOT_TUPLE:
    format = "Expected tuple, but got ";
    break;
  case ERR_NEG_INDEX:
    format = "Unexpected negative index ";
    break;
  case ERR_INDEX_OVERFLOW:
    format = "Index out of bounds ";
    break;
  case ERR_NOT_RECORD:
    format = "Expected record, but got ";
    break;
  case ERR_RECORD_TYPE:
    format = "Got record with incorrect type: ";
    break;
  case ERR_ARITY_MISMATCH:
    fprintf(stderr, "Arity mismatch: function expects ");
    printval(stderr, v);
    fprintf(stderr, " arguments\n");
    goto end;
    break;
  case ERR_NOT_CLOSURE:
    format = "Expected a function, got ";
    break;
  default:
    fprintf(stderr, "Unknown error code: %d", err);
    goto end;
    break;
  }

  fprintf(stderr, format);
  printval(stderr, v);
  fprintf(stderr, "\n");

end:
  exit(err);
}

/* DEFAULT FOREIGN FUNCTIONS */
val print(val v) {
  printf("> ");
  printval(stdout, v);
  printf("\n");
  return v;
}

val raw_print(val v){
  printf("> (raw) 0x%016lx\n", v);
  return v;
}

/* USER DEFINED FOREIGN FUNCTIONS */
val min(val v1, val v2){
  return v1 < v2 ? v1 : v2;
}

val min_of_8(val v1, val v2, val v3, val v4, val v5, val v6, val v7, val v8){
  return min(min(min(v1, v2), min(v3, v4)), min(min(v5, v6), min(v7, v8)));
}

val xor(bool_v v1, bool_v v2){
  return v1 ^ v2;
}

val first_tup(int_v *a){
  return a[1]>>1;
}

/* GC */
val* HEAP_START;
val* HEAP_END;
val* HEAP_MID;
val* TO_SPACE;
val* FROM_SPACE;
val* ALLOC_PTR = 0;
val* SCAN_PTR = 0;
val* STACK_BOTTOM = 0;

/**
 * Sets the value of stack bottom
**/
void set_stack_bottom(val* stack_bottom) {
  STACK_BOTTOM = stack_bottom;
}

/**
 * Is Heap Pointer?
 * Returns TRUE if v is in range [Hstart, Hend]
**/
bool is_heap_ptr(val v){
  return (val *) v < HEAP_END && (val*) v >= HEAP_START;
}

/**
 * Indicates if a pointer is an instance of forwadding address
 * (ie: if is a pointer to to-space)
**/
bool is_forwadding_addr(val * ptr){
  // | FROM_SPACE | TO_SPACE |
  if (TO_SPACE > FROM_SPACE)
    return HEAP_MID < ptr && ptr < HEAP_END;
  // | TO_SPACE | FROM_SPACE |
  else
    return HEAP_START < ptr && ptr < HEAP_MID;
}

/**
 * Prints the content in current stack frame
 * recursively calls untils it gets to the bottom
**/
void print_stack(val* rbp, val* rsp) {

  printf("|===============================================\n");
  printf("|     frame %p to %p   \n", rsp, rbp);
  printf("|===============================================\n");
  
  for (val* cur_word = rsp; cur_word < rbp; cur_word++) {
    val v = (val) *cur_word;
    printf("|-- %p: %p ", cur_word, (val*) *cur_word);
    if (is_heap_ptr(v)) {
      printval(stdout, v);
    }
    printf("\n");
  }
  if (rbp < STACK_BOTTOM) {
    print_stack((val*) *rbp, rbp + 2);
  }
  else {
    printf("|======================================\n");
    printf("|       bottom %p      \n", STACK_BOTTOM);
    printf("|======================================\n");
  }
}

/**
 * Prints the content in heap from 
 * heap_start to heap_end
**/
void print_heap(val* heap_start, val* heap_end){
  printf("|-> Heap from %p to %p\n", heap_start, heap_end);
  for (val i = 0; i <= (val) (heap_end - heap_start); i++) {
    printf("|--  %lld/%p: %p \n", i, (heap_start + i), (val*)*(heap_start + i));
  }
}

/**
 * Prints the content in heaps 
 * (from-space & to-space)
**/
void print_heaps(){
  printf("\n");
  printf("|======================\n");
  printf("|        HEAP 1        \n");
  printf("|======================\n");
  print_heap(HEAP_START, HEAP_MID-1);
  printf("|======================\n");
  printf("|        HEAP 2        \n");
  printf("|======================\n");
  print_heap(HEAP_MID, HEAP_END);
  printf("|=======================\n\n");
}

/**
 * Makes a copy of a tuple in alloc pointer
**/
val copy_tuple(val v){
  // Saves and tags the addres
  val addr = ((val) ALLOC_PTR | TAG_TUPLE);
  // Obtains tuple as array
  val * tup = TUPLE_TO_ARRAY(v);
  val size = GET_TUPLE_SIZE(v);
  // First, copy the size
  *ALLOC_PTR++ = *VAL_TO_PTR(v);
  // And now on, all the elements
  for (int i = 0; i < size; i++){
    *ALLOC_PTR++ = tup[i];
  }
  return addr;
}

/**
 * Makes a copy of a closure in alloc pointer
**/
val copy_closure(val v){
  // Saves and tags the addres
  val addr = ((val) ALLOC_PTR | TAG_CLOSURE);
  // Obtains the closure
  val * closure = VAL_TO_PTR(v);
  printf("| copying closure\n");
  // First, copy the arity
  printf("|   arity: %d\n", (*closure)>>1);
  *ALLOC_PTR++ = *closure++;
  // Then, copy the address in code (label)
  printf("|   address: %.16lx\n", *closure);
  *ALLOC_PTR++ = *closure++;
  // After that, the number of free vars
  int free_vars = *closure;
  printf("|   enclosed ids: %d\n", free_vars);
  *ALLOC_PTR++ = *closure++;
  // And now on, all the free vars values
  for (int i = 0; i < free_vars; i++){
    printf("|   enclosed value: %.16lx\n", *closure);
    *ALLOC_PTR++ = *closure++;
  }
  return addr;
}

/**
 * Makes a copy of a record in alloc pointer
**/
val copy_record(val v){
  // Saves and tags the addres
  val addr = ((val) ALLOC_PTR | TAG_RECORD);
  // First copy the header
  *ALLOC_PTR++ = *VAL_TO_PTR(v);
  // Then, the elements
  int size = GET_RECORD_SIZE(v);
  val* rec = RECORD_TO_ARRAY(v);
  for (int i = 0; i < size; i++){
    *ALLOC_PTR++ = rec[i];
  }
  return addr;
}

/**
 * Makes a copy of objects in from-space
 * if is a forwadding address returns itself
**/
val copy(val v){
  val* ptr = (val*) v;
  if (!is_forwadding_addr(ptr)){
    switch (typeofval(v)){
      case TYPE_TUPLE:
        return copy_tuple(v);
      case TYPE_CLOSURE:
        return copy_closure(v);
      case TYPE_RECORD:
        return copy_record(v);
      default:
        break;
    }
  }
  return v;
}

val* collect(val* cur_frame, val* cur_sp) {
  /* TBD: see https://en.wikipedia.org/wiki/Cheney%27s_algorithm */

  // init pointers
  ALLOC_PTR = TO_SPACE; // TO_SPACE IS EMPTY
  SCAN_PTR  = TO_SPACE; // TO_SPACE IS EMPTY

  val* ptr;

  // for root in stack
  for (val* cur_word = cur_sp; cur_word < cur_frame; cur_word++) {
    val v = (val) *cur_word;    // The value in stack
    if (is_heap_ptr(v)) {       // If pointer to heap
      *cur_word = copy(v);      // Creates a copy in to-space
    }
  }

  // Scanning objects in heap
  while (SCAN_PTR < ALLOC_PTR){
    val v = *SCAN_PTR;
    if (is_heap_ptr(v)) {       // If pointer to heap
      *SCAN_PTR = copy(v);      // Creates a copy in to-space
    }
    SCAN_PTR++;
  }
  
  // Cleaning from-space
  val * tmp = FROM_SPACE;
  val * END = (val) HEAP_MID > (val) FROM_SPACE ? HEAP_MID : HEAP_END;
  while (tmp < END) {
    *tmp++ = 0;
  }

  tmp = FROM_SPACE;
  FROM_SPACE = TO_SPACE;
  TO_SPACE = tmp;
  
  return ALLOC_PTR;
}

/* trigger GC if enabled and needed, out-of-memory error if insufficient */
val* try_gc(val* alloc_ptr, int words_needed, val* cur_frame, val* cur_sp) {
  // WIP: collect function
  if (USE_GC==1 && alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) {
    //printf("| Before GC \n");
    //print_heaps();
    printf("| need memory: GC!\n");
    alloc_ptr = collect(cur_frame, cur_sp);
    //print_heaps();
  }
  if (alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) {
    printf("| Error: out of memory!\n\n");
    print_stack(cur_frame, cur_sp);
    print_heaps();
    exit(-1);
  }
  return alloc_ptr;
}

/* start */
int main(int argc, char** argv){

  /* stack size config */
  char* stack_size_envvar = getenv("STACK_SIZE");
  if (stack_size_envvar) STACK_SIZE = strtoull(stack_size_envvar, NULL, 0);
  printf("| Setting stack size to %" PRId64 " .\n", STACK_SIZE);
  struct rlimit limit;
  getrlimit(RLIMIT_STACK, &limit);
  limit.rlim_cur = STACK_SIZE < limit.rlim_max ? STACK_SIZE : limit.rlim_max;
  int res = setrlimit(RLIMIT_STACK, &limit);
  if (res != 0) { printf("| Setting rlimit failed...\n") ; }

  /* GC config */
  char* use_gc_envvar = getenv("USE_GC");
  if (use_gc_envvar) USE_GC = strtoull(use_gc_envvar, NULL, 0);
  printf("| Use GC: %d\n", USE_GC);

  /* heap size config */
  char* heap_size_envvar = getenv("HEAP_SIZE");
  if (heap_size_envvar) HEAP_SIZE = strtoull(heap_size_envvar, NULL, 0);
  printf("| Heap size: %" PRId64 " .\n", HEAP_SIZE);

  /* setting up two space heap for GC */
  val* heap = (val*)calloc((HEAP_SIZE * 2) + 15, sizeof(val));
  HEAP_START = (val*)(((val)heap + 15) & ~0xF);

  HEAP_MID    = HEAP_START + HEAP_SIZE;     // Middle of HEAP
  HEAP_END    = HEAP_START + HEAP_SIZE*2;   // End of HEAP
  FROM_SPACE  = HEAP_START;       // Initial From Space
  TO_SPACE    = HEAP_MID;     // Initial To Space

  /* Go! */
  val result = our_code_starts_here(HEAP_START);
  char *str = malloc(charcount(result));
  sprintval(str, result);
  printf("%s\n", str);
  free(str);

  /* Q: when do you need to call `free(heap)`? */
  /* Here */
  free(heap);

  return 0;
}


/**
 * OLD (DEAD) CODE
 * Not useful anymore
 * -- DELETE BEFORE SUBMIT --
**/

/* MAIN */
// int main(int argc, char** argv) {
//   uint64_t * HEAP = calloc(1024, sizeof(uint64_t));

//   if (!HEAP){
//     fprintf(stderr, "Heap space allocation failed");
//     exit(-1);
//   }

//   val result = our_code_starts_here(HEAP);

//   char *str = malloc(charcount(result));
//   sprintval(str, result);
//   printf("%s\n", str);
//   free(str);

//   free(HEAP);
//   return 0;
// }
