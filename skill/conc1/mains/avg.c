#include <stdio.h>

#define NX 345
#define NY 355

int main(void)
{
  FILE *fin, *fout;
  float arin[NY][NX], arout[NY][NX];
  int i, j, k;
  char fn[3];
  float x, y;

  sprintf(fn, "a%02d",1);
  fin = fopen(fn, "r");
  fread(arin, sizeof(float), NX*NY, fin);
  for (j = 0; j < NY; j++) {
    for (k = 0; k < NX; k++) {
       arout[j][k] = arin[j][k];
    }
  }
  fclose(fin);

  for ( i = 2; i < 31; i++) {
    sprintf(fn, "a%02d",i);
    fin = fopen(fn, "r");
    fread(arin, sizeof(float), NX*NY, fin);
    for (j = 0; j < NY; j++) {
      for (k = 0; k < NX; k++) {
         arout[j][k] += arin[j][k];
      }
    }
    fclose(fin);
  }

  for (j = 0; j < NY; j++) {
    for (i = 0; i < NX; i++) {
       arout[j][i] = arout[j][i] / 30;
    }
  }
  
  fout = fopen("avgout", "w");
  x = (float) NX ;
  y = (float) NY ;
  fwrite(&x, sizeof(float), 1, fout);
  fwrite(&y, sizeof(float), 1, fout);
  fwrite(arout, sizeof(float), NX*NY, fout);

  return 0;

}
