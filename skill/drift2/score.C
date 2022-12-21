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
void rms(float *odist, float *odir, float *fdist, float *fdir, int npts, 
         float &meandist, float &meandir, float &rmsdist, float &rmsdir, float &errad) ;
float delta_direction(float x, float y) ;
void dirfix(float *x, float y, int n) ;

#define MAXBUOYS 80000
int main(int argc, char *argv[]) {
  matchup list[MAXBUOYS];
  float odir[MAXBUOYS], odist[MAXBUOYS], fdir[MAXBUOYS], fdist[MAXBUOYS];
  FILE *fin;
  int i, j, ret, nbuoy, count;
  int fday, skpt, month, year, maxdays;
  float ia, correl, vcc;
  float b0, b1;
  float meandist, meandir, rmsdist, rmsdir, errad;

  if (argc < 3) {
    printf("Need 2 arguments: file to score, max forecast lead\n");
    return 1;
  }
  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file\n");
  }
  maxdays = atoi(argv[2]);
  ret = 1;
  for (i = 0; i < MAXBUOYS && ret != 0; i++) {
     ret = list[i].read(fin);
  }
  nbuoy = i - 1;

////////// Everything all at once:
  for (i = 0; i < nbuoy; i++) {
    odir[i] = list[i].obs_dir;
    odist[i] = list[i].obs_dist;
    fdir[i] = list[i].fcst_dir;
    fdist[i] = list[i].fcst_dist;
  }

  ssanaly_(odist, odir, fdist, fdir, nbuoy, ia, correl, vcc);
  printf("%5d %2d %6.3f %6.3f %6.3f",nbuoy, fday, ia, correl, vcc);
  fit_(fdist, odist, nbuoy, b0, b1, correl);
  printf("  %6.2f %6.3f", b0, b1);
  rms(odist, odir, fdist, fdir, nbuoy, meandist, meandir, rmsdist, rmsdir, errad);
  printf("  %6.2f %6.1f  %5.1f %5.1f %5.1f\n",meandist, meandir, rmsdist, rmsdir, errad);

////////// Separate by forecast day:
  printf("Count fday IA r VCC  B0 B1  mean d, theta, rms d, theta\n");
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
      rms(odist, odir, fdist, fdir, count, meandist, meandir, rmsdist, rmsdir, errad);
      printf("  %6.2f %6.1f  %5.1f %5.1f %5.1f\n",meandist, meandir, rmsdist, rmsdir, errad);
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
      printf("  %6.2f %6.3f", b0, b1);
      rms(odist, odir, fdist, fdir, count, meandist, meandir, rmsdist, rmsdir, errad);
      printf("  %6.2f %6.1f  %5.1f %5.1f %5.1f\n",meandist, meandir, rmsdist, rmsdir, errad);
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
        rms(odist, odir, fdist, fdir, count, meandist, meandir, rmsdist, rmsdir, errad);
        printf("  %6.2f %6.1f  %5.1f %5.1f %5.1f\n",meandist, meandir, rmsdist, rmsdir, errad);
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
void rms(float *odist, float *odir, float *fdist, float *fdir, int npts, 
         float &meandist, float &meandir, float &rmsdist, float &rmsdir, float &errad) {
  int i;
  float deldir, dx, dy;

  meandist = 0;
  rmsdist  = 0;
  for (i = 0; i < npts; i++) {
    meandist += odist[i] - fdist[i];
    rmsdist  += (odist[i] - fdist[i])*(odist[i] - fdist[i]);
  } 
  meandist /= (float) npts;
  rmsdist  = sqrt(rmsdist/(float) npts);

  meandir = 0.;
  rmsdir  = 0.;
  errad   = 0.;
  for (i = 0; i < npts; i++) {
     deldir = delta_direction(odir[i], fdir[i]);
     meandir += deldir;
     rmsdir  += deldir*deldir;
     dx = (odist[i] - fdist[i])*cos(deldir*M_PI/180.);
     dy = (odist[i] - fdist[i])*cos(deldir*M_PI/180.);
     errad += sqrt(dx*dx + dy*dy);
  }
  meandir /= (float) npts;
  rmsdir   = sqrt(rmsdir/(float) npts);
  errad   /= (float) npts;

  return;
} 
float delta_direction(float x, float y) {
  float tmp;
  tmp = x - y;
  if (tmp > 180) {
    tmp = tmp - 360.;
  }
  if (tmp < -180) {
    tmp = 360 + tmp;
  }
  //printf("x, y, tmp %f %f %f\n", x, y, tmp);
  return tmp;
}
void dirfix(float *x, float y, int n) {
  int i;
  for (i = 0; i < n; i++) {
    x[i] -= y;
    if (x[i] > 360.) x[i] -= 360.;
    if (x[i] < 0.)   x[i] += 360.;
  }
  return;
}
  
