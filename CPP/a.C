#include "ncepgrids.h"

int main(void) {
  #ifdef DEBUG
    printf("hello from debug\n"); fflush(stdout);
  #endif
  printf("hello from main\n"); fflush(stdout);

  return 0;
}
