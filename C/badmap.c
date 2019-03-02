#include <stdio.h>


int main(int argc, char *argv[])
{
  FILE *fout, *fout2;
  int nx, ny;
  unsigned char map[465][385], map2[355][345];
  int i, j;

  fout = fopen(argv[1],"w");
  nx = 385;
  ny = 465; 
  for (j = 0; j < ny; j++) {
    for (i = 0; i < nx; i++) {
      map[j][i] = 166;
    }
  }
  fwrite(map, sizeof(unsigned char), nx*ny, fout);

  fout = fopen(argv[2],"w");
  nx = 345;
  ny = 355; 
  for (j = 0; j < ny; j++) {
    for (i = 0; i < nx; i++) {
      map2[j][i] = 166;
    }
  }
  fwrite(map2, sizeof(unsigned char), nx*ny, fout);

  return 0;

}
