#include <stdio.h>

#define NX 345
#define NY 355

int main(int argc, char *argv[])
{
  FILE *fin, *fout;
  float arin[NY][NX], arout[NY][NX];
  int count[NY][NX];
  int i, j, k;
  char fn[3];
  float x, y;
  int ndays;

  ndays = atoi(argv[1]);

  sprintf(fn, "s%02d",1);
  fin = fopen(fn, "r");
  if (fin == NULL) { printf("Failed to open input file\n"); return -1; }

  fread(arin, sizeof(float), NX*NY, fin);
  for (j = 0; j < NY; j++) {
    for (k = 0; k < NX; k++) {
       if (arin[j][k] < 128 ) {
         arout[j][k] = arin[j][k];
         count[j][k] = 1;
       }
       else {
         arout[j][k] = 0;
         count[j][k] = 0;
       }
    }
  }
  fclose(fin);

  for ( i = 2; i < ndays; i++) {
    sprintf(fn, "s%02d",i);
    fin = fopen(fn, "r");
    if (fin == NULL) { printf("Failed to open input file\n"); return -1; }

    fread(arin, sizeof(float), NX*NY, fin);
    for (j = 0; j < NY; j++) {
      for (k = 0; k < NX; k++) {
       if (arin[j][k] < 128 ) {
         arout[j][k] += arin[j][k];
         count[j][k] += 1;
       }
      }
    }
    fclose(fin);
  }

  for (j = 0; j < NY; j++) {
    for (i = 0; i < NX; i++) {
       if (count[j][i] > 5) {
         arout[j][i] = arout[j][i] / count[j][i];
       }
       else {
         arout[j][i] = 0;
       }
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
