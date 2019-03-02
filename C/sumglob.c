#include <stdio.h>

#define NX 360
#define NY 180

int main(int argc, char *argv[])
{
  FILE *fin1, *fin2, *fout;
  int i, j, nx=NX, ny=NY;
  int base;
  unsigned char map1[NY][NX], map2[NY][NX];
  float flag[NY][NX];
  float sumdel[NY][NX];
  int dy, a, b, mm;
  char fname[80];
  int nday[12]={31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

  base=atoi(argv[1]);
  mm = base % 100 - 1;

  for (j = 0; j < ny; j++) {
    for (i = 0; i < nx; i++) {
      sumdel[j][i] = 0;
      flag[j][i] = 0;
    }
  }

  for (dy = 1; dy <= nday[mm]; dy++) {
    sprintf(fname, "flag.%4d%02d",base, dy);
    fin1 = fopen(fname, "r");
    if ( fin1 == NULL) {
      printf("Failed to open %s\n", fname);
    }

    i = fread(&map1[0][0], sizeof(unsigned char), nx*ny, fin1);
    if ( i != NX*NY ) { 
      printf("Only read in %d bytes from the first file\n", i);
      return -1;
    }
    i = fread(&map2[0][0], sizeof(unsigned char), nx*ny, fin1);
    if ( i != NX*NY ) { 
      printf("Only read in %d bytes from the first file\n", i);
      return -1;
    }

    fclose(fin1);

    for (j = 0; j < ny; j++) {
      for (i = 0; i < nx; i++) {
         sumdel[j][i] += map1[j][i] - 128;
         if (map2[j][i] == 128) flag[j][i] += 1;
      }
    }

  }

  sprintf(fname, "globdel.%d", base); 
  fout = fopen(fname, "w");
  fwrite(&sumdel[0][0], sizeof(float), nx*ny, fout);
  fclose(fout);

  sprintf(fname, "globflag.%d", base); 
  fout = fopen(fname, "w");
  fwrite(&flag[0][0], sizeof(float), nx*ny, fout);
  fclose(fout);

   
  return 0;
}
