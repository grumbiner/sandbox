#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_quarter<short int> in;
  global_quarter<float> out;
  global_quarter<double> sum, tmp;
  int i, count;

  sum.set(0.0);

  count = 0;

  for (i = 2; i < argc; i++) {
    fin = fopen(argv[i],"r");
    in.binin(fin);
    fclose(fin);
    count++; 
    conv(in, tmp);
    sum += tmp;
  }
  sum /= (double) count;
  conv(sum, out);
  printf("%s %d avg stats: %f %f %f %f\n",argv[1], count, 
              out.gridmax(), out.gridmin(), out.average(), out.rms() );

  fout = fopen(argv[1],"w");
  out.binout(fout);
  fclose(fout);

  return 0;
} 
