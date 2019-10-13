#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(int argc, char *argv[]) {
  float rdist, ralpha, rtime;
  float table[18];
  int j;
  float tmp;

  table[0] = 200;
  table[1] = 300;
  table[2] = 400;
  table[3] = 600;
  table[4] = 800;
  table[5] = 1000;
  table[6] = 1200;
  table[7] = 1600;
  table[8] = 2000;
  table[9] = 2400;
  table[10] = 3200;
  table[11] = 4000;
  table[12] = 5000;
  table[13] = 6400;
  table[14] = 8000;
  table[15] = 10000;
  table[16] = 15000;
  table[17] = 10*(5280/(39.37/12));

  rdist = atof(argv[1]);
  ralpha = atof(argv[2]);
  rtime  = atof(argv[3]);
  if (rdist/rtime > 10.) {
    rtime *= 60.;
  }
  printf("dist %f alpha %f time %f\n", rdist, ralpha, rtime);
  for (j = 17; j >=0; j--) {
    tmp = rtime*pow((table[j]/rdist),ralpha); 
    printf("%5.0f  %6.1f  %6.2f\n",table[j], tmp, tmp/60.); 
  }

  return 0;
}
