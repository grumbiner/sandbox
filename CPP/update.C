#include "ncepgrids.h"

// Compare RTG 30' and 30' gridded+accumulated navy retrievals
// Make notes, remarks about differences
// Robert Grumbine 6 Feb 2009

int main(int argc, char *argv[] ) {
  FILE *finrtg, *fingrid, *finage, *finmask, *finice;
  FILE *fout;

  global_ice<float> rtg, fill, del, icec;
  global_ice<unsigned char> age, mask;

  ijpt loc, tloc;
  latpt ll;
  float rgmin = 265.0, toler = 2.5, tcrit = 275.15;
  bool critflop;

// Get data:
  finrtg = fopen(argv[1], "r");
  if (finrtg == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[1]);
    return 1;
  }
  rtg.binin(finrtg);
  fclose(finrtg);

  fingrid = fopen(argv[2], "r");
  if (fingrid == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[2]);
    return 2;
  }
  fill.binin(fingrid);
  fclose(fingrid);

  finage = fopen(argv[3],"r");
  if (finage == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[3]);
    return 3;
  }
  age.binin(finage);
  fclose(finage);

  finmask = fopen(argv[4], "r");
  if (finmask == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[4]);
    return 4;
  }
  mask.binin(finmask);

  finice = fopen(argv[5], "r");
  if (finice == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[5]);
    return 5;
  }
  icec.binin(finice);
  icec *= 100;
/////////////////////

// simple checks:
  printf("rtg max, min, avg, rms %f %f %f %f\n",rtg.gridmax(), 
                    rtg.gridmin(), rtg.average(), rtg.rms() );
  printf("fill max, min, avg, rms %f %f %f %f\n",fill.gridmax(rgmin),
                    fill.gridmin(rgmin), fill.average(rgmin), fill.rms(rgmin) );
  printf("age max, min, avg, rms %d %d %d %d\n",age.gridmax(),
                    age.gridmin(), age.average(255), age.rms(255) );
  printf("ice max, min, avg, rms %4.0f %4.0f %4.0f %4.0f\n",icec.gridmax(),
                    icec.gridmin(), icec.average(224), icec.rms(224) );


// Take a look at the low resolution grids ------------------------------
  for (loc.j = 0; loc.j < age.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < age.xpoints(); loc.i++) {
    ll = del.locate(loc);

    if (fill[loc] != rgmin) {
      del[loc] = rtg[loc] - fill[loc];
      if (fabs(del[loc]) > toler || (rtg[loc] > tcrit && icec[loc] >= 50 && icec[loc] < 157) ) {
        critflop = false;
        if ( (fill[loc] > tcrit && rtg[loc] < tcrit) ||
             (fill[loc] < tcrit && rtg[loc] > tcrit)  ) critflop = true;
        printf("low %6.2f %6.2f  %1d %3.0f %6.2f %6.2f  %6.2f %1d  %3d\n",
                   ll.lat, ll.lon, mask[loc], icec[loc], 
                   fill[loc], rtg[loc], del[loc], critflop, age[loc]);
        if (mask[loc] == 1 || mask[loc] == 3) {
          if (icec[loc] == 0 && abs(del[loc]) > toler) {
            rtg[loc] = fill[loc];
          }
          else if (icec[loc] > 0 && icec[loc] < 157 && rtg[loc] > tcrit) {
            rtg[loc] = min(tcrit - 0.01, fill[loc]);
          }
          else {
            rtg[loc] = fill[loc];
          }
          printf("updated %6.2f %6.2f to %6.2f  del %6.2f\n",ll.lat, ll.lon, rtg[loc], del[loc]);
        }
      }
    }
    else {
      del[loc] = rgmin;
    }

// Split out sea ice test -- do test whether or not there have been sst observations:
    if (ll.lat >= 67 && ll.lat <= 70 &&
      ll.lon >= 240 && ll.lon <= 260) {
      printf("archi %f %f  %d %f %f %f\n",ll.lat, ll.lon, mask[loc], rtg[loc], fill[loc], icec[loc]);
    }

    if (icec[loc] > 50 && icec[loc] < 157 && rtg[loc] > tcrit) {
      if (fill[loc] == rgmin) fill[loc] = 273.15 - 1.8;
      del[loc] = rtg[loc] - min(tcrit - 0.01, fill[loc]);
      rtg[loc] = min(tcrit - 0.01, fill[loc]);
      printf("updated %6.2f %6.2f to %6.2f  del %6.2f\n",ll.lat, ll.lon, rtg[loc], del[loc]);
    }
  }
  }

  fout = fopen(argv[6],"w");
  rtg.ftnout(fout);
  fclose(fout);

  printf("dellow max, min, avg, rms %f %f %f %f\n",del.gridmax(rgmin), 
                  del.gridmin(rgmin), del.average(rgmin), del.rms(rgmin) );

  return 0;
}
