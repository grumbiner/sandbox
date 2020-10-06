#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_12th<float> climo_today, climo_m1day, climo_m2days;
  global_12th<float> guess, analy_m1day, analy_m2days;
  global_12th<float> ar1, ar2;

  fin = fopen(argv[1], "r");
  ar1.binin(fin);
  ar2.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  climo_today.binin(fin);
  fclose(fin);
  fin = fopen(argv[3],"r");
  climo_m1day.binin(fin);
  fclose(fin);
  fin = fopen(argv[4],"r");
  climo_m2days.binin(fin);
  fclose(fin);

  fin = fopen(argv[5], "r");
  analy_m1day.binin(fin);
  fclose(fin);
  fin = fopen(argv[6], "r");
  analy_m2days.binin(fin);
  fclose(fin);

  analy_m1day -= climo_m1day;
  analy_m1day *= ar1;
  
  analy_m2days -= climo_m2days;
  analy_m2days *= ar2;

  guess = climo_today;
  guess += analy_m1day; // at this point, damped anomaly
  guess += analy_m2days;

  fout = fopen(argv[7], "r");
  guess.binout(fout);
  fclose(fout);

  return 0;
}
