#include <stdio.h>

int main(void)
{
  FILE *bin, *bout;
  int nx=385, ny=465;
  int i, j;
  int ii, jj;
  float dum, conc[ny][nx], del[ny][nx];
  float tx, ty;
  unsigned char cout[ny][nx];

  bin = fopen("b2out.960331", "r");
  bout = fopen("b2ref","w");

  for ( j = 0; j < ny; j++) {
    for ( i = 0; i < nx; i++) {
      fscanf(bin, "%3d%3d%7.2f%7.2f%8.3f\n", &ii, &jj, &dum, &tx, &ty);
      printf("%3d%3d%7.2f%7.2f%8.3f %3d %3d\n",ii, jj, dum, tx, ty);
      cout[jj][ii] = (int)( tx*100. + 0.5);
    }
  }

  fwrite(cout, sizeof(unsigned char), nx*ny, bout);

  return 0;

}
