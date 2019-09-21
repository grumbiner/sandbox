#include <stdio.h>

/* Convert an binary unsigned char array to ascii */
/* Robert Grumbine */
/* Last Modified 29 May 1996 */

int main(int argc, char *argv[])
{
  int nx=69, ny=71;
  unsigned char a0[ny][nx];
  FILE *fin, *fout;
  int i, j;

  fin  = fopen(argv[1],"r");
  fout = fopen(argv[2],"w");
  fread(a0, sizeof(unsigned char), nx*ny, fin);

  for (j = 0; j < ny; j++) {
  for (i = 0; i < nx; i++) {
    fprintf(fout,"%3d %3d %7.2f\n", i, j, ((float)a0[j][i]) / 100.) ;
  }
  }

  return 0;
}
