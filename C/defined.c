#include <stdio.h>
/* Illustration of how to use the 'defined' macro for conditional 
   compilation */
/* Robert Grumbine 16 March 1998 */ 

#define Y
#if defined(X) || defined(Y)
  #define YES 23
#else
  #define YES 0
#endif

int main() {
  printf("%d\n",YES);
  return 0;
}
