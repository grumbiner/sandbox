#include <stdio.h>

#define NPTS 13

int main(int argc, char *argv[]) {
  FILE *finref, *fin1, *fin2, *fin3, *fin4;
  float tmp, ref[NPTS], a1[NPTS], a2[NPTS], a3[NPTS], a4[NPTS];
  int i, dum;
  char sdum[900];

  finref = fopen("ex2", "r");
  fin1 = fopen(argv[1],"r");
  fin2 = fopen(argv[2],"r");
  fin3 = fopen(argv[3],"r");
  fin4 = fopen(argv[4],"r");

  for (i = 0; i < NPTS; i++) {
    fscanf(finref,"%d %f\n",&dum, &tmp);
    ref[i] = tmp; tmp = 0.;
    fscanf(fin1," %f \n",&tmp);
    fprintf(stderr,"%f\n",tmp);
    a1[i] = tmp;
    fscanf(fin2," %f \n",&tmp);
    a2[i] = tmp;
    fscanf(fin3," %f \n",&tmp);
    a3[i] = tmp;
    fscanf(fin4," %f \n",&tmp);
    a4[i] = tmp;
    printf("%3d %f  %e %e %e %e\n",i, ref[i], a1[i]/ref[i]-1., a2[i]/ref[i]-1., a3[i]/ref[i]-1., a4[i]/ref[i]-1.);
  }

  return 0;
}
