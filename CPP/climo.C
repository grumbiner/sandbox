#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  gaussian<float> input(382);
  gaussian<double> tmp(382), tmp2(382);
  gaussian<double> sum(382), sum2(382);
  int i, allcount = 0, count = 0;
// Note that we'll be reading in monthly files, with hourly data --
//   npts varying, then, from 28*24 to 31*24.
// This has us work with 360 files, versus > 250,000 for hourly.
// drawback is that we have to look for end of file
// n.b.: the hourly T382 binary is ca. 2 Gb/month, so will proceed monthly
//   (!)
//
  sum.set(0.0);
  sum2.set(0.0);

  for (i = 1; i < argc; i++) {
    printf("opening %s\n",argv[i]);
    fin = fopen(argv[i],"r");
    count = 0;
    while (!feof(fin)) {
      if (input.ftnin(fin)) {
        if (!feof(fin)) {
          conv(input, tmp);
          tmp -= 273.15; // convert to C for numerical reasons
          tmp2 = tmp;
          tmp2 *= tmp;
  
          sum  += tmp;
          sum2 += tmp2;
  
          count++;
          allcount++;
        }
      }
    }
    printf("count %d = %d %d\n",i,count, allcount);
    fclose(fin);
  }

  fin = fopen("fout","w");
  sum.binout(fin);
  sum2.binout(fin);
  fclose(fin);

  return 0;
}
