#include <stdio.h>

#define NX 720
#define NY 360

int main(int argc, char *argv[])
{
  FILE *fin1, *fin2, *fout;
  int i, j, nx=NX, ny=NY;
  float map[NY][NX], sum[NY][NX];
  unsigned char cout[NY][NX];
  unsigned char count[NY][NX];
  int dy, a, b;
  char fnames[80];
  float div;

  for (j = 0; j < ny; j++) {
    for (i = 0; i < nx; i++) {
      sum[j][i] = 0;
      count[j][i] = 0;
    }
  }

  for (dy = 1; dy <= 30; dy++) {
    sprintf(fnames, "latlon.9606%02d", dy);
    fin1 = fopen(fnames, "r");
    if ( fin1 == NULL) {
      printf("Failed to open %s\n", fnames);
    }
    i = fread(&map[0][0], sizeof(float), nx*ny, fin1);

    if ( i != NX*NY ) { 
      printf("Only read in %d bytes from the first file\n", i);
      return -1;
    }

    for (j = 0; j < ny; j++) {
      for (i = 0; i < nx; i++) {
         sum[j][i] += map[j][i] ;
         if (map[j][i] >= 0.55) {
           count[j][i] += 1;
         }
      }
    }
    fclose(fin1);

  }

   printf("Now starting to average\n");

   if ( sum[0][0] > 15.*100.) {
     div = 30.;
     printf("100 based concs\n");
   }
   else {
     div = 0.3;
     printf("1.00 based concs\n");
   }

    for (j = 0; j < ny; j++) {
      for (i = 0; i < nx; i++) {
         cout[j][i] = (unsigned char) (sum[j][i]/div + 0.5) ;
      }
    }

  fout = fopen("sum", "w");
  fwrite(&cout[0][0], sizeof(unsigned char), nx*ny, fout);
  fout = fopen("count", "w");
  fwrite(&count[0][0], sizeof(unsigned char), nx*ny, fout);
   
  return 0;
}
