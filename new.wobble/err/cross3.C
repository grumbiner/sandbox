#include "mvector.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  time_series<float> glob(10104), nh(10104), sh(10104);
  int i;
  float tg, tn, ts;
  char s[4];

  fin = fopen(argv[1],"r");
  i = 0;
  while ( !feof(fin)) {
    fscanf(fin, "%f %f %f\n",&tg, &tn, &ts);
    glob[i] = tg;
    nh[i] = tn;
    sh[i] = ts;
    i++;
  }
  fclose(fin);

  for (i = 0; i < 2927; i++) {
    printf("%d %f %f %f   %f %f %f   %f %f %f\n",i, 
    glob.crosscorrel(glob, i),
    nh.crosscorrel(glob, i),
    sh.crosscorrel(glob, i),
    glob.crosscorrel(nh, i),
    nh.crosscorrel(nh, i),
    sh.crosscorrel(nh, i),
    glob.crosscorrel(sh, i),
    nh.crosscorrel(sh, i),
    sh.crosscorrel(sh, i)

   );

  }

  return 0;
} 
