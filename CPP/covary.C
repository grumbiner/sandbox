#include "time_series.h"

int main(int argc, char *argv[]) {
  time_series<float> rtg, obs, delta;
  float t1, t2, d;
  int jd, i;
  FILE *fin;
  char line[900], dummy[900];
  fin = fopen(argv[1], "r");

  i = 0;
  while (!feof(fin) && i <= 1679) {
    fscanf(fin,"%10c %f %d %f %f\n",dummy, &t1, &jd, &t2, &d);
    //printf("i = %d t1 = %f %d %f %f\n",i,t1,jd, t2, d);
    rtg[i] = t1;
    obs[i] = t2;
    delta[i] = d;
    i++;
  }
  t1 = rtg.average();
  rtg -= t1;
  t1 = obs.average();
  obs -= t1;
  t1 = delta.average();
  delta -= t1;

  float a1, a2, a3;
  float a10, a20, a30;
  a10 = 1;
  a20 = 1;
  a30 = 1;
  for (i = 0; i < 366; i++) {
    a1 = rtg.autocovary(i);
    a2 = obs.autocovary(i);
    a3 = delta.autocovary(i);
    printf("autocovariances %3d %f %f %f  cross %f %f\n",i,a1/a10, a2/a20, a3/a30,
         rtg.crosscorrel(rtg, i), rtg.crosscorrel(obs, i) );
  }
    

  return 0;
}
