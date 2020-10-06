#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_12th<float> in;
  global_12th<double> sum, tmp;
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
  conv(sum, in);
  printf("%s %d avg stats: %f %f %f %f\n",argv[1], count, 
              in.gridmax(), in.gridmin(), in.average(), in.rms() );

  fout = fopen(argv[1],"w");
  in.binout(fout);
  fclose(fout);

  return 0;
} 
