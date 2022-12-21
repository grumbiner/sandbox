#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_quarter<float> fl, days;
  float flag = 0.0;
  float lag;
  int i;
  
  fin  = fopen(argv[1], "r");
  fout = fopen(argv[2], "w");
  lag  = atof(argv[3]);

  for (i = 0; i < lag; i++) {
    fl.binin(fin);
  }

  days.set((float)0.0);
  for (i = 0; i < fl.xpoints()*fl.ypoints(); i++) {
    if (fl[i] > 0.0) {
      days[i] = 1./(1.-pow(fl[i],(float) (1./lag) ) );
    }
    else {
      if (fl[i] < 0.0) days[i] = lag;
    }
  }
  days.binout(fout);
  fclose(fout);

  printf("days stats %f %f %f %f\n",days.gridmax(flag), days.gridmin(flag), days.average(flag), days.rms(flag) );


  return 0;
}
