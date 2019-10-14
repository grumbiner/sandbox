#include <stdio.h>

#define NX 360
#define NY 180

int main(int argc, char *argv[])
{
  FILE *fin1, *fin2, *fout;
  int i, j, nx=NX, ny=NY;
  unsigned char map1[NY][NX], map2[NY][NX];
  int sumdel[NY][NX];
  int dy, a, b;
  char fnames[80];

  for (j = 0; j < ny; j++) {
    for (i = 0; i < nx; i++) {
      sumdel[j][i] = 0;
    }
  }

  for (dy = 3; dy <= 10; dy++) {
    sprintf(fnames, "flag.9112%02d", dy);
    fin1 = fopen(fnames, "r");
    if ( fin1 == NULL) {
      printf("Failed to open %s\n", fnames);
    }
    i = fread(&map1[0][0], sizeof(unsigned char), nx*ny, fin1);

    if ( i != NX*NY ) { 
      printf("Only read in %d bytes from the first file\n", i);
      return -1;
    }

    for (j = 0; j < ny; j++) {
      for (i = 0; i < nx; i++) {
         sumdel[j][i] += map1[j][i] - 128;
      }
    }

  }

  fout = fopen("sumdel.n", "w");
  fwrite(&sumdel[0][0], sizeof(int), nx*ny, fout);
   
  return 0;
}

