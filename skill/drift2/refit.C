#include <stdio.h>
#include <stdlib.h>
#include <math.h>

class matchup {
  public:
     int year, month, day;
     int skpt, lead;
     float lat1, lon1, lat2, lon2;
     float obs_dist, obs_dir, fcst_dist, fcst_dir;

     matchup(void);
     int read(FILE *);
};
matchup::matchup(void) {
  year = 98; month = 1; day = 1;
  skpt = 0; lead = 0;
  lat1 = 0.; lon1 = 0.; lat2 = 0.; lon2 = 0.;
  obs_dir = 0.; obs_dist = 0.;
  fcst_dir = 0.; fcst_dist = 0.;
}
int matchup::read(FILE *fin) {
  char id[6];
  id[5] = '\0';
  if ( feof(fin) ) {
    return 0;
  }
  fscanf(fin, "%d %d %d %d %d %f %f to %f %f %f %f %f %f %s\n",
    &year, &month, &day, &skpt, &lead, &lat1, &lon1, &lat2, &lon2, 
    &obs_dist, &obs_dir, &fcst_dist, &fcst_dir, &id);
  #ifdef VERBOSE
  printf(
   "%2d %2d %2d %3d %2d %5.2f %6.2f to %5.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
    year, month, day, skpt, lead, lat1, lon1, lat2, lon2, 
    obs_dist, obs_dir, fcst_dist, fcst_dir);
  #endif
  return 1;
}

  
extern "C" void ssanaly_(float *odist, float *odir, float *dist, float *dir, 
                              int &npts, float &ia, float &r2, float &vcc);
extern "C" void fit_(float *odist, float *dist, int &n, float &b0, float &b1, 
                     float &correl);
void detrnd(float *fdist, const float b0, const float b1, const int n);
void findcurrent(int skpt, float *odir, float *odist, float *fdir, float *fdist, float *lead, int count);

#define MAXBUOYS 80000
int main(int argc, char *argv[]) {
  matchup list[MAXBUOYS];
  float odir[MAXBUOYS], odist[MAXBUOYS], fdir[MAXBUOYS], fdist[MAXBUOYS];
  float lead[MAXBUOYS];
  float current;
  FILE *fin;
  int i, j, ret, nbuoy, count;
  int fday, skpt, month, year, maxdays;
  float ia, correl, vcc;
  float b0, b1;

  if (argc < 4) {
    printf("Need 3 arguments: file to score, max forecast lead, and year\n");
    return 1;
  }
  fin = fopen(argv[1], "r");
  maxdays = atoi(argv[2]);
  year = atoi(argv[3]);
  ret = 1;
  for (i = 0; i < MAXBUOYS && ret != 0; i++) {
     ret = list[i].read(fin);
  }
  nbuoy = i - 1;

////////// Split by point, compute currents per point, and then proceed with
////////// normal scoring
  for (skpt = 1; skpt <= 207; skpt++) {
    count = 0;
    for (i = 0; i < nbuoy; i++) {
      if (list[i].skpt == skpt ) {
        odir[count]  = list[i].obs_dir;
        odist[count] = list[i].obs_dist;
        fdir[count]  = list[i].fcst_dir;
        fdist[count] = list[i].fcst_dist;
        lead[count]  = list[i].lead;
        count += 1;
      }
    }
    count -= 1;
    if (count > 10) {
      // find current - vector and subtract from forecast, return modified fcst
      findcurrent(skpt, odir, odist, fdir, fdist, lead, count);
      count = 0;
      for (i = 0; i < nbuoy; i++) {
        if (list[i].skpt == skpt ) {
          list[i].fcst_dir  = fdir[count];
          list[i].fcst_dist = fdist[count];
          count += 1;
        }
      }
    }
  }


////////// Separate by forecast day:
  printf("Count fday IA r VCC  B0 B1  detrended IA r VCC\n");
  for (fday = 1; fday <= maxdays; fday++) {
    count = 0;
    for (i = 0; i < nbuoy; i++) { 
      if (list[i].lead == fday) {
        odir[count] = list[i].obs_dir;
        odist[count] = list[i].obs_dist;
        fdir[count] = list[i].fcst_dir;
        fdist[count] = list[i].fcst_dist;
        count += 1;
      }
    }
    count -= 1;
    if (count >= 10) {
      ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
      printf("%5d %2d %6.3f %6.3f %6.3f",count, fday, ia, correl, vcc);
      fit_(fdist, odist, count, b0, b1, correl);
      printf("  %6.2f %6.3f", b0, b1);
      detrnd(fdist, b0, b1, count);
      ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
      printf("  %6.3f %6.3f %6.3f\n",ia, correl, vcc);
    }
    else {
      printf("%4d %2d Insufficient data\n", count, fday);
    }
  }

//Separate by forecast month:
  printf(" \n");
  printf("Count mon IA r VCC  B0 B1  \n");
  for (month = 1; month <= 12; month++) {
    count = 0;
    for (i = 0; i < nbuoy; i++) { 
      //if (list[i].month == month && list[i].lead == 6 ) {
      if (list[i].month == month ) {
        odir[count] = list[i].obs_dir;
        odist[count] = list[i].obs_dist;
        fdir[count] = list[i].fcst_dir;
        fdist[count] = list[i].fcst_dist;
        count += 1;
      }
    }
    count -= 1;
    if (count >= 10) {
      ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
      printf("%5d %2d %6.3f %6.3f %6.3f",count, month, ia, correl, vcc);
      fit_(fdist, odist, count, b0, b1, correl);
      printf("  %6.2f %6.3f\n", b0, b1);
    }
    else {
      printf("%5d %2d Insufficient data\n", count, month);
    }
  }

//Separate by skiles point
  printf("\n Skiles point verification\n");
  printf("count fday IA r VCC B0 B1 IA' R' VCC' \n");
  // Note that fday is now skiles point
  for (skpt = 1; skpt <= 207; skpt++) {
    for (fday = 1; fday <= maxdays; fday++) {
      count = 0;
      for (i = 0; i < nbuoy; i++) { 
        if (list[i].skpt == skpt && list[i].lead == fday ) {
          odir[count] = list[i].obs_dir;
          odist[count] = list[i].obs_dist;
          fdir[count] = list[i].fcst_dir;
          fdist[count] = list[i].fcst_dist;
          count += 1;
        }
      }
      count -= 1;
      if (count > 3) {
        printf("pt %3d day %2d  ",skpt, fday);
        ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
        printf("%5d %2d %6.3f %6.3f %6.3f",count, fday, ia, correl, vcc);
        fit_(fdist, odist, count, b0, b1, correl);
        printf("  %6.2f %6.3f", b0, b1);
        detrnd(fdist, b0, b1, count);
        ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
        printf("  %6.3f %6.3f %6.3f\n",ia, correl, vcc);
      }
    }
  }

  return 0;
}
void detrnd(float *fdist, const float b0, const float b1, const int count) {
  int i;
  for (i = 0; i < count; i++) {
    fdist[i] = b0 + b1 * fdist[i];
  }
  return;
}
void findcurrent(int skpt, float *odir, float *odist, float *fdir, 
                   float *fdist, float *lead, int count) {
  float dx, dy;
  float curx, cury, dir, speed;
  float sumx, sumy, sumt2;
  int i;

  sumx = 0; sumy = 0; sumt2 = 0;
  for (i = 0; i < count; i++) {
     dx = (fdist[i] - odist[i])*cos(M_PI/180.*(fdir[i] - odir[i]));
     dy = (fdist[i] - odist[i])*sin(M_PI/180.*(fdir[i] - odir[i]));
     sumx = dx * lead[i];
     sumy = dy * lead[i];
     sumt2 = lead[i]*lead[i];
  }
  curx = sumx / sumt2;
  cury = sumy / sumt2;
  speed = sqrt(curx*curx + cury*cury);
  dir   = atan2(cury, curx) * 180./M_PI;
  for (i = 0; i < count; i++) {
     fdist[i] += speed*lead[i];
     fdir[i]  += dir;
     if (fdir[i] > 360.) fdir[i] -= 360.;
     if (fdir[i] < 0.)   fdir[i] += 360.;
  }
  printf("point %3d speed %4.1f direction %6.1f\n",skpt, speed, dir);

  return;
}
