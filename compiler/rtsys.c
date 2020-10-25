#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

typedef int64_t val;

extern val our_code_starts_here() asm("our_code_starts_here");

const val BOOL_BITMASK = 0x0000000000000001;
const val BOOL_TRUE = 0x8000000000000001;
const val BOOL_FALSE = 0x0000000000000001;

void print_value(val v) {
  if (!(v & BOOL_BITMASK)) { // integer
    printf("%ld", v >> 1);
  } else if (v == BOOL_TRUE) {
    printf("true");
  } else if (v == BOOL_FALSE) {
    printf("false");
  } else {
    printf("Unknown value: %#018x", v);
  }
}

char * value_to_str(val v){
  if (!(v & BOOL_BITMASK)) { // integer
    return "%ld", v >> 1;
  } else if (v == BOOL_TRUE) {
    return "true";
  } else if (v == BOOL_FALSE) {
    return "false";
  } else {
    return "Unknown value: %#018x", v;
  }
}

const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
// other error codes here

void error(int errCode, val v) {
  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "Expected number, but got %s\n", value_to_str(v));
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Expected boolean, but got %010x\n", v);
  }
  exit(errCode);
}

int main(int argc, char** argv) {
  val result = our_code_starts_here();
  print_value(result);
  return 0;
}
