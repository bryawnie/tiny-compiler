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

int main(int argc, char** argv) {
  val result = our_code_starts_here();
  print_value(result);
  return 0;
}
