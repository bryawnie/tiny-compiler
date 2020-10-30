#include <stdio.h>
#include <stdint.h>

typedef uint64_t val;   // any value
typedef int64_t int_v;  // signed integer
typedef uint64_t bool_v;// boolean

extern val our_code_starts_here() asm("our_code_starts_here");


/* Bitmasks */

const val BOOL_BITMASK = 0x0000000000000001;
const val BOOL_TRUE = 0x8000000000000001;
const val BOOL_FALSE = 0x0000000000000001;


/* Error codes */

const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;

char * value_to_str(val v){
  char * strOut = (char *) malloc(21*sizeof(char));
  if (!(v & BOOL_BITMASK)) { // integer
    sprintf(strOut, "%ld", (int_v) v >> 1);
    return strOut;
  } else if (v == BOOL_TRUE) {
    return "true";
  } else if (v == BOOL_FALSE) {
    return "false";
  } else {
    sprintf(strOut, "Unknown value: %#018lx", v);
    return strOut;
  }
}

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

/* DEFAULT FOREIGN FUNCTIONS */

val print(val v) {
  if (!(v & BOOL_BITMASK)) { // integer
    printf("> %ld\n", v >> 1);
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
  val result = our_code_starts_here();
  printf(value_to_str(result));
  return 0;
}
