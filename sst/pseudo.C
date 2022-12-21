#include "ncepgrids.h"

typedef struct {
  latpt ll;
  ijpt loc;
  float climo, rtg, delta;
  unsigned char mask;
  float dist;
} sst_check;

int main(int argc, char *argv[]) {
  global_12th<float> distance;
  global_12th<unsigned char> mask;
  float lon, lat, climo, rtg, delta;
  ijpt loc;
  latpt ll;
  sst_check tmpob;
  mvector<sst_check> sstob(25000);
  FILE *fin;
  int i, j, nr, nobs, days;
  float t, dt, rate = 3.0;

  fin = fopen("seaice_alldist.bin","r");
  distance.binin(fin);
  distance /= 1000.; // convert to km
  fclose(fin);
  fin = fopen("seaice_gland5min","r");
  mask.binin(fin);
  fclose(fin);

// do the basic read in 
  fin = fopen(argv[1],"r");

  i = 0;
  while (!feof(fin)) {
    fscanf(fin, "%f %f %f %f %f %f\n",&lon, &lat, &climo, &rtg, &delta, &t);
    tmpob.ll.lon = lon;
    tmpob.ll.lat = lat;
    tmpob.loc    = mask.locate(tmpob.ll);

    tmpob.climo  = climo;
    tmpob.rtg    = rtg;
    tmpob.delta  = delta;

    tmpob.mask   = mask[tmpob.loc];
    tmpob.dist   = distance[tmpob.loc];

    sstob[i] = tmpob;
    //printf("%7.3f %7.3f  %4d %4d   %5.2f %5.2f %6.2f     %6.2f\n",
    //        sstob[i].ll.lon, sstob[i].ll.lat, sstob[i].loc.i, sstob[i].loc.j, 
    //        sstob[i].climo, sstob[i].rtg, sstob[i].delta, sstob[i].dist);

    i++;
  }
  nobs = i;
  //printf("nobs = %d\n",nobs);

  rate = atof(argv[2]);
  for (i = 0; i < nobs; i++) {
    loc = sstob[i].loc;
    delta = sstob[i].delta;
    if (delta > 0) delta -= 4.5;
    if (delta < 0) delta += 4.5;

    days = ceil(fabs(delta)/rate);
    if (days == 0) continue;
    if (fabs(delta) < 1.0 ) continue; // if it's this close, let obs try again.
    dt = - delta / days;

    printf("%7.3f %7.3f  %5.2f %5.2f %6.2f     %7.2f days: %2d dt: %5.2f  ",
            sstob[i].ll.lon, sstob[i].ll.lat, 
            sstob[i].climo, sstob[i].rtg, sstob[i].delta, distance[loc], days, dt );

    t = sstob[i].rtg;
    for (j = 0; j < days; j++) {
      t += dt;
      printf("%5.2f ",t);
    }
    printf("\n");
  }

  return 0;
}
