#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef uint64_t val;   // any value
typedef int64_t int_v;  // signed integer
typedef uint64_t bool_v;// boolean

extern val our_code_starts_here(uint64_t * HEAP) asm("our_code_starts_here");


/* Bitmasks */

const val BOOL_BITMASK = 0x0000000000000007;
const val INT_BITMASK = 0x0000000000000001;
const val BOOL_TRUE = 0x8000000000000007;
const val BOOL_FALSE = 0x0000000000000007;


/* Error codes */

const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
const int ERR_NOT_TUPLE = 3;
const int ERR_NEG_INDEX = 4;
const int ERR_INDEX_OVERFLOW = 5;

char * strtuple(int *p){
  int size = *p;
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

char * strval(val v){
  char * strOut = (char *) malloc(30*sizeof(char));
  int lastBits = v & 7;
  int lastBit = v & 1;
  int dir = v & 0xfffffffffffffff8;
  if(!lastBit){
    return strintval(v);
  } else {
    switch (lastBits){
      case 7: // Boolean
        if (v == BOOL_TRUE) 
          return "true";
        return "false";
      case 1: // Tuple¿
        return strtuple((val *) dir);
      default:
        sprintf(strOut, "Unknown value: %#018lx", v);
        return strOut;
    }
  }
}

void error(int errCode, val v) {
  switch (errCode){
  case ERR_NOT_NUMBER:
    fprintf(stderr, "Expected number, but got %s\n", strval(v));
    break;
  case ERR_NOT_BOOLEAN:
    fprintf(stderr, "Expected boolean, but got %s\n", strval(v));
    break;
  case ERR_NOT_TUPLE:
    fprintf(stderr, "Expected tuple, but got %s\n", strval(v));
    break;
  case ERR_NEG_INDEX:
    fprintf(stderr, "Unexpected negative index %ld\n", (int_v) v);
    break;
  case ERR_INDEX_OVERFLOW:
    fprintf(stderr, "Index out of bounds %ld\n", (int_v) v);
    break;
  default:
    fprintf(stderr, "Unknown error code: %d", errCode);
    break;
  }
  exit(errCode);
}

/* DEFAULT FOREIGN FUNCTIONS */

val print(val v) {
  if (!(v & INT_BITMASK)) { // integer
    printf("> %ld\n", (int_v) v >> 1);
  } else if (v == BOOL_TRUE) {
    printf("> true\n");
  } else if (v == BOOL_FALSE) {
    printf("> false\n");
  } else {
    printf("> Unknown value: %#018lx\n", v);
  }
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
  printf(strval(result));
  free(HEAP);
  return 0;
}
