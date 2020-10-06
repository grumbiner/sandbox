#include "ncepgrids.h"

// Compute the gradient vector, magnitude, and laplacean of input file
// Write out each

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  char fname[900];
  int x[4];
  global_quarter<short int> day;
  global_quarter<float> sst;
  global_quarter<float> dx, dy, mag, lapl;
  float flag = -999, scale = 100.;
  ijpt loc, tloc;

  fin = fopen(argv[1],"r");
  fread(&x, sizeof(int), 4, fin);
  day.binin(fin);
  fclose(fin);
// flip to my convention:
    for (loc.j = 0; loc.j < day.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < day.xpoints(); loc.i++) {
      tloc.j = day.ypoints() - loc.j - 1;
      tloc.i = loc.i;
      sst[tloc] = day[loc];
    }
    }

  gradients(sst, dx, dy, mag, flag);
  laplacean(sst, lapl, flag);
  dx /= scale;
  dy /= scale;
  mag /= scale;
  lapl /= scale;

  sprintf(fname,"dx_%s",argv[2]);
  fout = fopen(fname, "w");
  dx.binout(fout);
  fclose(fout);

  sprintf(fname,"dy_%s",argv[2]);
  fout = fopen(fname, "w");
  dy.binout(fout);
  fclose(fout);

  sprintf(fname,"mag_%s",argv[2]);
  fout = fopen(fname, "w");
  mag.binout(fout);
  fclose(fout);

  sprintf(fname,"lapl_%s",argv[2]);
  fout = fopen(fname, "w");
  lapl.binout(fout);
  fclose(fout);

  dx *= 1.e5;
  dy *= 1.e5;
  mag *= 1.e5;
  lapl *= 1.e10;
  printf("maxima  %s %6.2f %6.2f %6.2f %9.2f\n",argv[2], dx.gridmax(), dy.gridmax(), mag.gridmax(), lapl.gridmax() );
  printf("minima  %s %6.2f %6.2f %6.2f %9.2f\n",argv[2], dx.gridmin(), dy.gridmin(), mag.gridmin(), lapl.gridmin() );
  printf("rms     %s %6.2f %6.2f %6.2f %9.2f\n",argv[2], dx.rms(), dy.rms(), mag.rms(), lapl.rms() );
  printf("average %s %6.2f %6.2f %6.2f %9.2f\n",argv[2], dx.average(), dy.average(), mag.average(), lapl.average() );

  return 0;
}  
