#include "ncepgrids.h"
// Apr 28  2009
// Robert Grumbine
// Scan amsr-e data file and look for bounds to use for coarse quality
//   control in sea ice algorithms.

#include "amsr.h"
#define NSPOT 1000
// Note, WARNING: amsr lats/lons may be off planet!

int main (int argc, char *argv[]) {
  global_12th<float> sst, ice;
  amsrept x[NSPOT];
  FILE *fin;
  int n, i, j;
  bool hotice = false;
  float gr24, gr37, gr89;
  latpt ll;
  ijpt loc;
  mvector<float> ice_tmax(14), ice_tmin(14);
  mvector<float> sst_tmax(14), sst_tmin(14);

  fin = fopen(argv[2], "r");
  sst.binin(fin);
  fclose(fin);

  fin = fopen(argv[3], "r");
  ice.binin(fin);
  fclose(fin);

  ice_tmax = 0.0;
  sst_tmax = 0.0;
  ice_tmin = 999.0;
  sst_tmin = 999.0;

  fin = fopen(argv[1], "r");
  while (!feof(fin)) {
    n = fread(x, sizeof(amsrept), NSPOT, fin);

    if (n > 0) {
      for (i = 0; i < n; i++) {
        ll.lat = (float) x[i].clat[0];
        ll.lon = (float) x[i].clon[0];
        if (fabs(ll.lat) > 90. || fabs(ll.lon) > 360.) continue;
        loc = sst.locate(ll);

        // for all points that are hot, see what tbs are like
        if (sst[loc] > (273.15 + 25.0) ) {
          #ifdef VERBOSE
          printf("hot %6.2f %7.2f  %5.2f %4.0f",ll.lat, ll.lon, 
                      sst[loc]-273.15, 100*ice[loc]);
          #endif
          for (j = 0; j < 14; j++) {
            #ifdef VERBOSE
              printf(" %5.1f",x[i].obs[j].tmbr);
            #endif
            if (x[i].obs[j].tmbr > sst_tmax[j] && x[i].obs[j].tmbr < 1e3) {
                   sst_tmax[j] = x[i].obs[j].tmbr;
            }
            if (x[i].obs[j].tmbr < sst_tmin[j] && x[i].obs[j].tmbr  > 0.) {
                   sst_tmin[j] = x[i].obs[j].tmbr;
            }
          }
          #ifdef VERBOSE
            printf("\n");
            fflush(stdout);
          #endif
        }

        // for all points that are ice, see what tbs are like
        if (ice[loc] > 0 && ice[loc] < 1.28) {
          hotice = false;
          #ifdef VERBOSE
            printf("ice %6.2f %7.2f  %5.2f %4.0f  ",ll.lat, ll.lon, 
                           sst[loc]-273.15, ice[loc]*100.);
          #endif
          for (j = 0; j < 14; j++) {
            #ifdef VERBOSE
              printf(" %5.1f",x[i].obs[j].tmbr);
            #endif
            if (x[i].obs[j].tmbr > ice_tmax[j] && x[i].obs[j].tmbr < 1e3) ice_tmax[j] = x[i].obs[j].tmbr;
            if (x[i].obs[j].tmbr < ice_tmin[j]) ice_tmin[j] = x[i].obs[j].tmbr;
              
            if (x[i].obs[j].tmbr > 273.15 && x[i].obs[j].tmbr  < 1e3) hotice = true;
          }
          #ifdef VERBOSE
            printf("\n");
            fflush(stdout);
          #endif

          if (hotice) {
            printf("hotice %6.2f %7.2f  %5.2f %4.0f  ",ll.lat, ll.lon, sst[loc]-273.15, ice[loc]*100.);
            for (j = 0; j < 14; j++) {
              printf(" %5.1f",x[i].obs[j].tmbr);
            }
            printf("\n");
            fflush(stdout);
          }

        }

//Verbose 2: print out everything, plus the GR values
        gr24 = (x[i].obs[AMSR_T24V].tmbr - x[i].obs[AMSR_T19V].tmbr) /
               (x[i].obs[AMSR_T24V].tmbr + x[i].obs[AMSR_T19V].tmbr)   ;
        gr37 = (x[i].obs[AMSR_T37V].tmbr - x[i].obs[AMSR_T19V].tmbr) /
               (x[i].obs[AMSR_T37V].tmbr + x[i].obs[AMSR_T19V].tmbr)   ;
        gr89 = (x[i].obs[AMSR_T89Va].tmbr - x[i].obs[AMSR_T19V].tmbr) /
               (x[i].obs[AMSR_T89Va].tmbr + x[i].obs[AMSR_T19V].tmbr)   ;
        printf("grs %6.2f %7.2f  %5.2f %4.0f  ",ll.lat, ll.lon, 
                sst[loc]-273.15, ice[loc]*100.);
        for (j = 0; j < 14; j++) {
          printf(" %5.1f",x[i].obs[j].tmbr);
        }
        printf(" %9.6f %9.6f %9.6f\n",gr24, gr37, gr89);
        fflush(stdout);

        if (ice[loc] > 0 && ice[loc] < 1.28 && sst[loc] > 275.15) {
          printf("bad %6.2f %7.2f  %5.2f %4.0f  ",ll.lat, ll.lon, 
                  sst[loc]-273.15, ice[loc]*100.);
          for (j = 0; j < 14; j++) {
            printf(" %5.1f",x[i].obs[j].tmbr);
          }
          printf("\n");
          fflush(stdout);
        }

      }
    }
  }

  printf("Max min, sst, ice\n");
  for (j = 0; j < 14; j++) {\
    printf("%2d  %5.1f %5.1f  %5.1f %5.1f\n",j, sst_tmax[j], sst_tmin[j], 
                                                ice_tmax[j], ice_tmin[j]);
  }

  return 0;
}
