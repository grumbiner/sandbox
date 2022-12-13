#include <stdio.h>
#define NX 720
#define NY 360

int main(void)
{
  FILE *fout;
  int i, j;
  unsigned char age[NY][NX];

  for (j = 0; j < NY; j++) {
    for (i = 0; i < NX; i++) {
      age[j][i] = 1;
    }
  }

  fout = fopen("age1","w");
  fwrite(age, sizeof(unsigned char), NY*NX, fout);

  return 0;
}
