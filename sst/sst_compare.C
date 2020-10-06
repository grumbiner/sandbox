#include "ncepgrids.h"

// Intercompare the RTG 30', RTG 5', simple-minded navy retrievals.
//   add info about the land mask and sea ice cover
// Robert Grumbine 8 Jan 2009

int main(int argc, char *argv[] ) {
  FILE *fin1, *fin2, *finland, *fin1hr, *fin2hr, *finlandhr;
  FILE *fout;

  global_ice<float> navy, rtg, del, ice;
  global_ice<int> count;
  global_ice<unsigned char> iceland, rtgland;
  global_12th<float> navyhr, rtghr, delhr, icehr;
  global_12th<float> fromlow, delrtg;
  global_12th<int> counthr;
  global_12th<unsigned char> icelandhr, rtglandhr, newland;

  ijpt loc, tloc;
  latpt ll;
  float rgmin = 265.0, hi_toler = 1.5, low_toler = 1.5, tcrit = 275.15;
  bool critflop;

// Get data:
  fin1 = fopen(argv[1], "r");
  if (fin1 == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[1]);
    return 1;
  }
  fin2 = fopen(argv[2], "r");
  if (fin2 == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[2]);
    return 2;
  }
  navy.binin(fin1);
  count.binin(fin1);
  rtg.binin(fin2);
  fclose(fin1);
  fclose(fin2);
  finland = fopen(argv[3],"r");
  if (finland == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[3]);
    return 3;
  }
  iceland.binin(finland);
  rtgland.binin(finland);
  fclose(finland);

  fin1hr = fopen(argv[4], "r");
  if (fin1hr == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[4]);
    return 4;
  }
  fin2hr = fopen(argv[5], "r");
  if (fin2hr == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[5]);
    return 5;
  }
  navyhr.binin(fin1hr);
  counthr.binin(fin1hr);
  rtghr.binin(fin2hr);
  fclose(fin1hr);
  fclose(fin2hr);
  finlandhr = fopen(argv[6],"r");
  if (finlandhr == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[6]);
    return 6;
  }
  icelandhr.binin(finlandhr);
  rtglandhr.binin(finlandhr);
  fclose(finlandhr);

  fin1 = fopen(argv[7], "r");
  if (fin1 == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[7]);
    return 7;
  }
  ice.binin(fin1);
  fclose(fin1);
  fin2 = fopen(argv[8], "r");
  if (fin2 == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[8]);
    return 8;
  }
  icehr.binin(fin2);
  fclose(fin2);
  fin2 = fopen(argv[9], "r");
  if (fin2 == (FILE *) NULL) {
    printf("Failed to read in %s\n",argv[9]);
    return 9;
  }
  newland.binin(fin2);
  fclose(fin2);


// simple checks:
  #ifdef VERBOSE
  printf("navy max, min, avg, rms %f %f %f %f\n",navy.gridmax(rgmin), 
                    navy.gridmin(rgmin), navy.average(rgmin), navy.rms(rgmin) );
  printf("rtg max, min, avg, rms %f %f %f %f\n",rtg.gridmax(), 
                    rtg.gridmin(), rtg.average(), rtg.rms() );
  printf("navyhr max, min, avg, rms %f %f %f %f\n",navyhr.gridmax(rgmin), 
                    navyhr.gridmin(rgmin), navyhr.average(rgmin), navyhr.rms(rgmin) );
  printf("rtghr max, min, avg, rms %f %f %f %f\n",rtghr.gridmax(), 
                    rtghr.gridmin(), rtghr.average(), rtghr.rms() );
  #endif

// Take a look at the low resolution grids ------------------------------
  for (loc.j = 0; loc.j < del.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < del.xpoints(); loc.i++) {
    if (navy[loc] != rgmin) {
      del[loc] = rtg[loc] - navy[loc];
      if (fabs(del[loc]) > low_toler) {
        ll = del.locate(loc);
        critflop = false;
        if ( (navy[loc] > tcrit && rtg[loc] < tcrit) ||
             (navy[loc] < tcrit && rtg[loc] > tcrit)  ) critflop = true;
        printf("low %6.2f %6.2f  %3d  %3d %3d  %3d  %6.2f %6.2f  %7.3f\n",
                   ll.lat, ll.lon, count[loc], 
                   iceland[loc], rtgland[loc], (int) (0.5 + 100.*ice[loc]), 
                   navy[loc], rtg[loc], del[loc]);
      }
    }
    else {
      del[loc] = rgmin;
    }
  }
  }

  printf("dellow max, min, avg, rms %f %f %f %f\n",del.gridmax(rgmin), 
                  del.gridmin(rgmin), del.average(rgmin), del.rms(rgmin) );
  //printf("global low area integral %e\n",del.integrate(rgmin) );

// Take a look at the high resolution grids ------------------------------
  // first, copy over from low res rtg to high res grid for add'l comparison
  for (loc.j = 0; loc.j < delhr.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < delhr.xpoints(); loc.i++) {
    ll = fromlow.locate(loc);
    tloc = rtg.locate(ll);
    fromlow[loc] = rtg[tloc];
  }
  } 

  for (loc.j = 0; loc.j < delhr.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < delhr.xpoints(); loc.i++) {
    if (navyhr[loc] != rgmin) {
      delhr[loc] = rtghr[loc] - navyhr[loc];
      if (fabs(delhr[loc]) > hi_toler) {
        ll = delhr.locate(loc);
        critflop = false;
        if ( (navyhr[loc] > tcrit && rtghr[loc] < tcrit) ||
             (navyhr[loc] < tcrit && rtghr[loc] > tcrit)  ) critflop = true;
        printf("hinav %6.2f %6.2f  %3d  %3d %3d %1d  %3d  %6.2f %6.2f %6.2f  %7.3f\n",
                   ll.lat, ll.lon, counthr[loc], 
                   icelandhr[loc], rtglandhr[loc], newland[loc], 
                   (int) (0.5 + 100.*icehr[loc]), 
                   fromlow[loc], navyhr[loc], rtghr[loc], delhr[loc]);
      }
    }
    else {
      delhr[loc] = rgmin;
    }

    delrtg[loc] = rtghr[loc] - fromlow[loc];
    if (fabs(delrtg[loc]) > hi_toler) {
        ll = delrtg.locate(loc);
        critflop = false;
        if ( (fromlow[loc] > tcrit && rtghr[loc] < tcrit) ||
             (fromlow[loc] < tcrit && rtghr[loc] > tcrit)  ) critflop = true;
        if (! (iceland[loc] == 157 && rtgland[loc] == 157 && newland[loc] == 2) ) {
          // don't print out if everyone agrees it is land
          printf("hirtg %6.2f %6.2f  %3d  %3d %3d %1d  %3d  %6.2f %6.2f %6.2f  %7.3f\n",
                   ll.lat, ll.lon, counthr[loc], 
                   icelandhr[loc], rtglandhr[loc], newland[loc], 
                   (int) (0.5 + 100.*icehr[loc]), 
                   fromlow[loc], navyhr[loc], rtghr[loc], delrtg[loc]);
        }
      }

  }
  }

  printf("delnav max, min, avg, rms %f %f %f %f\n",delhr.gridmax(rgmin), 
             delhr.gridmin(rgmin), delhr.average(rgmin), delhr.rms(rgmin) );
  printf("delrtg max, min, avg, rms %f %f %f %f\n",delrtg.gridmax(rgmin), 
             delrtg.gridmin(rgmin), delrtg.average(rgmin), delrtg.rms(rgmin) );

//----------------------- Sea Ice-related checks:
  float tcold = 273.15 - 1.0;

  for (loc.j = 0; loc.j < delhr.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < delhr.xpoints(); loc.i++) {
    if (icehr[loc] != 0) {
      if (navyhr[loc] > tcrit || rtghr[loc] > tcrit || fromlow[loc] > tcrit) {
        // analysis too warm for ice being present
        if (! (icelandhr[loc] == 157 && rtglandhr[loc] == 157 && newland[loc] == 2) ) {
          // don't print out if everyone agrees it is land
          ll = icehr.locate(loc);
          printf("icew  %6.2f %6.2f  %3d  %3d %3d %1d  %3d  %6.2f %6.2f %6.2f  %7.3f\n",
                   ll.lat, ll.lon, counthr[loc], 
                   icelandhr[loc], rtglandhr[loc], newland[loc], 
                   (int) (0.5 + 100.*icehr[loc]), 
                   fromlow[loc], navyhr[loc], rtghr[loc], delrtg[loc]);
        }
      }
    }
    if (icehr[loc] == 0 && icelandhr[loc] != 157 ) {
      if ((navyhr[loc] < tcold && navyhr[loc] > 269.0) || 
            rtghr[loc] < tcold || fromlow[loc] < tcold) {
        // analysis is cold for there being no ice present
        if (! (icelandhr[loc] == 157 && rtglandhr[loc] == 157 && newland[loc] == 2) ) {
          // don't print out if everyone agrees it is land
          ll = icehr.locate(loc);
          printf("icec  %6.2f %6.2f  %3d  %3d %3d %1d  %3d  %6.2f %6.2f %6.2f  %7.3f\n",
                   ll.lat, ll.lon, counthr[loc], 
                   icelandhr[loc], rtglandhr[loc], newland[loc], 
                   (int) (0.5 + 100.*icehr[loc]), 
                   fromlow[loc], navyhr[loc], rtghr[loc], delrtg[loc]);
        }
      }
    }
  }
  }

  return 0;
}
