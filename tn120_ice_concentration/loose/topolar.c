#include <stdio.h>

#define NX 385
#define NY 465

int main(int argc, char *argv[])
{

  FILE *fin, *fout;
  float x[NY][NX];
  int i, j;
  char fname[80];
  int k;

  for (k = 1; k <= 12; k++) {
    sprintf(fname,"acortime.96%02d",k);
    fin = fopen(fname, "r");
    fread(x, sizeof(float), NX*NY, fin);
    sprintf(fname,"texttime.96%02d",k);
    fout = fopen(fname, "w"); 

    for (j = 0; j < NY; j++) {
      for (i = 0; i < NX; i++) {
         fprintf(fout, "%3d %3d %6.2f\n",i,j,x[j][i]);
      }
    }
   
  }

  return 0;

}
  
