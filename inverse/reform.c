#include <stdio.h>

int main(int argc, char *argv[])
{
  FILE *bin, *bout;
  int nx=69, ny=71;
  int i, j;
  int ii, jj;
  float dum, tx, ty;
  unsigned char cout[ny][nx];

  bin = fopen(argv[1], "r");
  bout = fopen(argv[2], "w");

  ii = 0; jj = 0; dum = 0.; tx = 0.; ty = 0.;

  for ( j = 0; j < ny; j++) {
    for ( i = 0; i < nx; i++) {
/*      fscanf(bin, "%3d%3d%7.2f%7.2f%8.3f\0", &ii, &jj, &dum, &tx, &ty); */
      fscanf(bin, "%d%d%f%f%f\n", &ii, &jj, &dum, &tx, &ty);
      printf("%3d%3d%7.2f%7.2f%8.3f\n",ii, jj, dum, tx, ty);
      cout[jj][ii] = (int)( tx*100. + 0.5);
    }
  }

  fwrite(cout, sizeof(unsigned char), nx*ny, bout);

  return 0;

}
