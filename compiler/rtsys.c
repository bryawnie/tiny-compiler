#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define TRACE 1


typedef uint64_t val;   // any value
typedef int64_t int_v;  // signed integer
typedef uint64_t bool_v;// boolean

extern val our_code_starts_here(uint64_t * HEAP) asm("our_code_starts_here");


/* TYPE TAGS*/

const val TAG_BITMASK = 0x7;
const val TAG_INT = 0x0;
const val TAG_BOOL = 0x1;
const val TAG_TUPLE = 0x3;
const val TAG_RECORD = 0x5;

/* Constants */
const val BOOL_BIT = (0x1UL << 63);
const val RECORD_SIZE_BITMASK = ~0UL >> 32;
const val VAL_TRUE = BOOL_BIT | TAG_BOOL;
const val VAL_FALSE = TAG_BOOL;

/* MACROS */
#define VAL_TO_PTR(v) ((val*) (v & ~TAG_BITMASK))
#define GET_TUPLE_SIZE(v) (*VAL_TO_PTR(v))
#define TUPLE_TO_ARRAY(v) (VAL_TO_PTR(v) + 1)
/* the first element of a record contains two numbers: the 32 least
  significant bits represent its size, and the most significant 32 bits
  are a type tag to differentiate amongst different records. */
#define GET_RECORD_SIZE(v) (*VAL_TO_PTR(v) & RECORD_SIZE_BITMASK)
#define GET_RECORD_TYPE(v) (*VAL_TO_PTR(v) & ~RECORD_SIZE_BITMASK) >> 32
#define RECORD_TO_ARRAY(v) (VAL_TO_PTR(v) + 1)

/* Format strings */
const char *STR_TRUE = "true";
const char *STR_FALSE = "false";

enum error_code {
  NOERR,
  ERR_NOT_NUMBER,
  ERR_NOT_BOOLEAN,
  ERR_NOT_TUPLE,
  ERR_NEG_INDEX,
  ERR_INDEX_OVERFLOW,
  ERR_NOT_RECORD,
  ERR_RECORD_TYPE
};
typedef enum error_code errcode;

enum data_type {
  TYPE_INT,
  TYPE_BOOL,
  TYPE_TUPLE,
  TYPE_RECORD
};
typedef enum data_type dtype;

dtype typeofval(val v) {
  
  // fprintf(stderr, "typeofval(defsys raw_print any -> any): v=%ld\n", v);
   
  if (!(v & 1)) {
    // integer values are always even (last bit is 0)
    return TYPE_INT;
  }
  return (dtype) 1 + (v & TAG_BITMASK)/2;
}

/*
char * strtuple(int *p){
  int size = *p >> 1;
  char * strtmp = (char *) malloc(30*sizeof(char));
  char * tupStr = (char *) malloc(30*sizeof(char));
  sprintf(tupStr, "(tup");
  for (int i=1; i<=size;i++){
    sprintf(strtmp, " %ld", (int_v) *(p+2*i) >> 1 );
    strcat(tupStr, strtmp);
  }
  free(strtmp);
  strcat(tupStr,")");
  return tupStr;
}

char * strintval(val v){
  char * strOut = (char *) malloc(30*sizeof(char));
  sprintf(strOut, "%ld", (int_v) v >> 1);
  return strOut;
}
*/

int arr_charcount(val *v, int size);
/* Counts the maximum amount of characters required to print a value. */
int charcount(val v) {
  
  // fprintf(stderr, "charcount: v=%ld\n", v);
  

  switch (typeofval(v)) {

    case TYPE_INT:
      return 20;

    case TYPE_BOOL:
      return 5;

    case TYPE_TUPLE:
      return 2 + arr_charcount(VAL_TO_PTR(v) + 1, *VAL_TO_PTR(v));

    case TYPE_RECORD:
      return 2 + arr_charcount(VAL_TO_PTR(v) + 1, GET_RECORD_SIZE(v));

    default:
      return 40;
  }
}

int arr_charcount(val *a, int size) {
  
  /* fprintf(stderr, "arr_charcount: a=%p, size=%d\n", a, size); */
  

  int count = 0;
  for (int i = 0; i < size; i++) {
    count = count + charcount(a[i]) + 1;
  }
  return count;
}

int arr_sprintval(char *str, val* a, int size, char* prefix, char* suffix);

int sprintval(char *str, val v) {
  
  /* fprintf(stderr, "sprintval: str=%p, v=%ld\n", str, v); */
  char *aux;
  
  switch (typeofval(v)) {

    case TYPE_INT:
      return sprintf(str, "%ld", ((int_v) v) >> 1);

    case TYPE_BOOL:
      switch (v) {
        case VAL_TRUE:
        aux = STR_TRUE;
        break;
        case VAL_FALSE:
        aux = STR_FALSE;
        break;
        default:
        return sprintf(str, "Unknown value: %#018lx", v);
      }
      return sprintf(str, "%s", aux);

    case TYPE_TUPLE:
      return arr_sprintval(str, TUPLE_TO_ARRAY(v), GET_TUPLE_SIZE(v), "(", ")");

    case TYPE_RECORD:
      return arr_sprintval(str, RECORD_TO_ARRAY(v), GET_RECORD_SIZE(v), "{", "}");

    default:
      return sprintf(str, "Unknown value: %#018lx", v);
  }
}

int arr_sprintval(char *str, val *a, int size, char* pre, char* post) {
  /* 
  fprintf(stderr, "arr_sprintval: str=%p, a=%p, size=%d, pre='%s', post='%s'",
    str, a, size, pre, post);
   */

  char *s = str;
  s = s + sprintf(s, "%s", pre);
  for (int i = 0; i < size; i++) {
    s = s + sprintval(s, a[i]);
    if (i + 1 < size)
      *s++ = ' ';
  }
  s = s + sprintf(s, "%s", post);
  return s - str;
}

char *val_to_str(val v) {
  
  // fprintf(stderr, "val_to_str: %ld\n", v);
  
  int str_size = charcount(v);
  char *str = malloc(str_size);
  sprintval(str, v);

  return str;
}

void error(int errCode, val v) {
  
  //fprintf(stderr, "error: %ld\n", v);

  switch (errCode){
  case ERR_NOT_NUMBER:
    fprintf(stderr, "Expected number, but got %s\n", val_to_str(v));
    break;
  case ERR_NOT_BOOLEAN:
    fprintf(stderr, "Expected boolean, but got %s\n", val_to_str(v));
    break;
  case ERR_NOT_TUPLE:
    fprintf(stderr, "Expected tuple, but got %s\n", val_to_str(v));
    break;
  case ERR_NEG_INDEX:
    fprintf(stderr, "Unexpected negative index %s\n", val_to_str(v-2));
    break;
  case ERR_INDEX_OVERFLOW:
    fprintf(stderr, "Index out of bounds %s\n", val_to_str(v-2));
    break;
  case ERR_NOT_RECORD:
    fprintf(stderr, "Expected record, but got %s\n", val_to_str(v));
    break;
  case ERR_RECORD_TYPE:
    fprintf(stderr, "Got record with incorrect type: %s\n", val_to_str(v));
    break;
  default:
    fprintf(stderr, "Unknown error code: %d", errCode);
    break;
  }
  exit(errCode);
}

/* DEFAULT FOREIGN FUNCTIONS */
val print(val v) {
  // char *str = val_to_str(v); // This segaults for some reason
  char *str;
  switch (typeofval(v)) {
    case TYPE_INT:
    printf("> %ld\n", ((int_v) v) >> 1);
    break;

    case TYPE_BOOL:
    printf("> %s\n", (v & BOOL_BIT) ? STR_TRUE : STR_FALSE);
    break;

    case TYPE_TUPLE:
    /* str = val_to_str(v);
    printf("> %s\n", str);
    free(str); */
    printf("> (tuple)\n");
    break;

    case TYPE_RECORD:
    /*
    str = val_to_str(v);
    printf("> %s\n", str);
    free(str);
    */
    printf("> {record}\n");
    break;

    default:
    printf("> Unknown value: %#018lx", v);
    break;
  }
  // free(str);
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

/* MAIN */
int main(int argc, char** argv) {
  uint64_t * HEAP = calloc(1024, sizeof(uint64_t));
  val result = our_code_starts_here(HEAP);
  char *result_str = val_to_str(result);
  printf(result_str);
  free(result_str);
  free(HEAP);
  return 0;
}
