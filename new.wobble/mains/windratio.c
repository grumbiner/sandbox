#include <stdio.h>

int main(int argc, char *argv[]) {
  FILE *finu, *finv;
  float t1, t2, t3, t4, t5, t6;
  float u[192][94], v[192][94];
  int i = 0, l = 0, m = 0, n = 0, j;
  int ngrid = 18, nx = 192;

  finu = fopen(argv[1],"r");
  finv = fopen(argv[2], "r");

  while ( !feof(finu) ) {
    fscanf(finu, "%e %e %e %e %e %e\n",&t1, &t2, &t3, &t4, &t5, &t6);
    //printf("%g %g %g %g %g %g\n",t1, t2, t3, t4, t5, t6);
    n = i / (ngrid*nx);
    l = i % ngrid;
    m = (i-n*ngrid*nx) / ngrid;
    //printf("u l m n j %3d %3d %3d %3d %e\n",l, m, n, l+n*ngrid, t5); fflush(stdout);
    u[m][l+n*ngrid] = t5;

    fscanf(finv, "%e %e %e %e %e %e\n",&t1, &t2, &t3, &t4, &t5, &t6);
    l = i % 192;
    m = i / 192;
    //printf("v i j %d %d %e\n",l, m, t5); fflush(stdout);
    v[l][m] = t5;
    //printf("%e %e\n",v[l][m], t5);

    i++;
  }

  //for (j = 0; j < 94; j++) {
  for (j = 0; j < 90; j++) {
  for (i = 0; i < 192; i++) {
    printf("%3d %2d  %e %e  %f\n",i, j, u[i][j], v[i][j], u[i][j]/v[i][j]);
  }
  }

  return 0;
}
