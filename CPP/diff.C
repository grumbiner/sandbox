#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_quarter<short int> qdint;
  global_quarter<float> qdoi;
  global_12th<float> expected, oi;
  FILE *fin;
  float landval = -9, maskval = -9;

  fin = fopen(argv[1], "r");
  qdint.binin(fin);
  fclose(fin);
  conv(qdint, qdoi);
  qdoi /= 100.;
  //printf("qdoi %f %f %f %f\n",qdoi.gridmax(), qdoi.gridmin(), qdoi.average(), qdoi.rms() );
  oi.fromall(qdoi, landval, maskval);
  //printf("oi %f %f %f %f\n",oi.gridmax(), oi.gridmin(), oi.average(), oi.rms() );

  fin = fopen(argv[2], "r");
  expected.ftnin(fin);
  fclose(fin);
  //printf("expect %f %f %f %f\n",expected.gridmax(), expected.gridmin(), expected.average(), expected.rms() );

  expected -= oi;
  printf("expect - obsd %f %f %f %f\n",expected.gridmax(), expected.gridmin(), expected.average(), expected.rms() );

  fin = fopen(argv[3], "w");
  expected.binout(fin);
  fclose(fin);

  return 0;
}
