#include <stdio.h>

int main(void) {
  FILE *fout;
  float x = 0.333333333333;
  fout = fopen("tmp","r");
  fread(&x, sizeof(float), 1, fout);
  printf("x = %f\n",x);
  fclose(fout);
 
  return 0;
}
