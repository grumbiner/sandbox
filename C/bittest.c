#include <stdio.h>

int main(void)
{
  struct { unsigned int a : 16;
             unsigned int b : 16;
             unsigned int c : 16;
             unsigned int d : 16;
           } x;
  FILE *fout;

  x.a = 0;
  x.b = 100*100;
  x.c = 200*100;
  x.d = 400*100;

  printf("%d %d %d %d \n",x.a, x.b, x.c, x.d);
 
  fout=fopen("ftest","w");
  fwrite(&x, sizeof(x), 1, fout);
 

  return 0;
}              
