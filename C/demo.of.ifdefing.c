#include <stdio.h>

#define VERBOSE
#undef FRED

#ifdef  VERBOSE && FRED
  #define XXX 1
#else
  #ifdef  VERBOSE | FRED 
    #define XXX 2
  #else
    #define XXX 3
  #endif
#endif 

int main(void) {
  printf("%d x\n",XXX);
  return 0;
}
