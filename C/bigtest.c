#include <stdio.h>
#include <stdlib.h>

#define TYPE char

int main(void) {
  TYPE *x;
  int i;

  printf("size of type %ld\n",sizeof(TYPE));
  fflush(stdout);

  for (i = 1;  i <= 1024*1024; i*= 2 ) {
    printf("%d Mb\n",i); 
    fflush(stdout);
    x = malloc(sizeof(TYPE)*i*1024*1024);
    free(x);
  }

  return 0;
} 
