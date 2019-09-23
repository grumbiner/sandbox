#include <stdio.h>

#undef VERBOSE
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
  if ( feof(fin) ) {
    return 0;
  }
  fscanf(fin, "%d %d %d %d %d %f %f to %f %f %f %f %f %f\n",
    &year, &month, &day, &skpt, &lead, &lat1, &lon1, &lat2, &lon2, 
    &obs_dist, &obs_dir, &fcst_dist, &fcst_dir);
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
extern "C" void drift3_(float *odist, float *odir, float *dist, float *dir,
                         int &npts, float &obar, float &errbar, 
                         float &errsd, float &err1bar, float &err2bar, 
                         float &t);

#define MAXBUOYS 27000
int main(int argc, char *argv[]) {
  matchup list[MAXBUOYS];
  float odir[MAXBUOYS], odist[MAXBUOYS], fdir[MAXBUOYS], fdist[MAXBUOYS];
  float avdist=0., nulldir[MAXBUOYS], nulldist[MAXBUOYS];
  FILE *fin;
  int i, j, ret, nbuoy, count;
  int fday, skpt, month, avindex;
  float ia, correl, vcc;
  float obar, errbar, errsd, err1bar, err2bar, t;

  fin = fopen(argv[1], "r");
  ret = 1;
  for (i = 0; i < MAXBUOYS && ret != 0; i++) {
     ret = list[i].read(fin);
  }
  nbuoy = i - 1;
  for (fday = 1; fday <= 16; fday++) {
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

    ssanaly_(odist, odir, fdist, fdir, count, ia, correl, vcc);
    printf("%3d %2d %6.3f %6.3f %6.3f ",count, fday, ia, correl, vcc);

    for (avindex = 0; avindex < count; avindex++) {
       avdist += odist[avindex];
    }
    avdist /= (float) count;
    for (i = 0; i < count; i++) {
        nulldir[count] = avdist;
        nulldist[count] = avdist;
    }
    ssanaly_(odist, odir, nulldist, nulldir, count, ia, correl, vcc);
    printf("null ia %6.3f  ",ia);
    drift3_(odist, odir, fdist, fdir, count, 
        obar, errbar, errsd, err1bar, err2bar, t);
    printf(" drift %5.1f %5.1f %5.1f  %5.3f\n",obar, errbar, errsd, errbar/obar);
  }

  return 0;
}
