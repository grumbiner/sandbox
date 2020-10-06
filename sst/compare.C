#include "ncepgrids.h"

// Intercompare the RTG 30', RTG 5', simple-minded navy retrievals.
//   add info about the land mask and sea ice cover
// Robert Grumbine 8 Jan 2009

// Start moving to a subroutine for comparisons 25 August 2010:
void compare(llgrid<float> &x, llgrid<float> &y, char *tag) ;
// version to ignore land -- xland is on the x (reference) grid, and non-land
//   is flagged by 0 (assumed)
void compare(llgrid<float> &x, llgrid<unsigned char> &xland, llgrid<float> &y, char *tag) ;

int main(int argc, char *argv[] ) {
  FILE *fin1, *fin2, *finland, *fin1hr, *fin2hr, *finlandhr;

  global_sst<float> oisst;
  global_sst<unsigned char> oiland;

  global_ice<float> navy, rtg, del, ice;
  global_ice<int> count;
  global_ice<unsigned char> iceland, rtgland;

  global_12th<float> navyhr, rtghr, delhr, icehr;
  global_12th<float> fromlow, delrtg;
  global_12th<int> counthr;
  global_12th<unsigned char> icelandhr, rtglandhr, newland;

  ijpt loc, tloc;
  latpt ll;
  float rgmin = 265.0, toler = 1.5, tcrit = 275.3;
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

  fin1 = fopen(argv[10],"r");
  if (fin1 == (FILE *) NULL ) {
    printf("Failed to read in %s\n",argv[10]);
    return 10;
  }
  oisst.binin(fin1);
  fclose(fin1);

  fin1 = fopen(argv[11],"r");
  if (fin1 == (FILE *) NULL ) {
    printf("Failed to read in %s\n",argv[11]);
    return 11;
  }
  oiland.binin(fin1);
  fclose(fin1);

// simple checks:
  #ifdef VERBOSE
  printf("navy   max, min, avg, rms %7.3f %7.3f %7.3f %7.3f\n",navy.gridmax(rgmin), 
                    navy.gridmin(rgmin), navy.average(rgmin), navy.rms(rgmin) );
  printf("rtg    max, min, avg, rms %7.3f %7.3f %7.3f %7.3f\n",rtg.gridmax(), 
                    rtg.gridmin(), rtg.average(), rtg.rms() );
  printf("navyhr max, min, avg, rms %7.3f %7.3f %7.3f %7.3f\n",navyhr.gridmax(rgmin), 
                    navyhr.gridmin(rgmin), navyhr.average(rgmin), navyhr.rms(rgmin) );
  printf("rtghr  max, min, avg, rms %7.3f %7.3f %7.3f %7.3f\n",rtghr.gridmax(), 
                    rtghr.gridmin(), rtghr.average(), rtghr.rms() );
  printf("oisst  max, min, avg, rms %7.3f %7.3f %7.3f %7.3f\n",oisst.gridmax(), 
                    oisst.gridmin(), oisst.average(), oisst.rms() );
  #endif


// Start working with the comparison subroutine
  compare(rtg,   iceland,   oisst, "lowvoi\0");
  compare(rtghr, icelandhr, oisst, "highvoi\0");
  compare(oisst, oiland,    rtg,   "oivlow\0");
  compare(oisst, oiland,    rtghr, "oivhigh\0");
  compare(rtg,   iceland,   rtghr, "lowvhigh\0");
  compare(rtghr, icelandhr, rtg,   "highvlow\0");


//// old methods:
// Take a look at the low resolution grids ------------------------------
  for (loc.j = 0; loc.j < del.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < del.xpoints(); loc.i++) {
    if (navy[loc] != rgmin) {
      del[loc] = rtg[loc] - navy[loc];
      if (fabs(del[loc]) > toler) {
        ll = del.locate(loc);
        critflop = false;
        if ( (navy[loc] > tcrit && rtg[loc] < tcrit) ||
             (navy[loc] < tcrit && rtg[loc] > tcrit)  ) critflop = true;
        printf("low %6.2f %6.2f  %3d   %3d  %6.2f %6.2f  %7.3f %1d\n",
                   ll.lat, ll.lon, count[loc], 
                   (int) (0.5 + 100.*ice[loc]), 
                   navy[loc], rtg[loc], del[loc], critflop);
      }
    }
    else {
      del[loc] = rgmin;
    }
  }
  }

  //printf("dellow max, min, avg, rms %f %f %f %f\n",del.gridmax(rgmin), 
  //                del.gridmin(rgmin), del.average(rgmin), del.rms(rgmin) );
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
      if (fabs(delhr[loc]) > toler) {
        ll = delhr.locate(loc);
        critflop = false;
        if ( (navyhr[loc] > tcrit && rtghr[loc] < tcrit) ||
             (navyhr[loc] < tcrit && rtghr[loc] > tcrit)  ) critflop = true;
        printf("hinav %6.2f %6.2f  %3d  %3d  %6.2f %6.2f %6.2f  %7.3f %1d\n",
                   ll.lat, ll.lon, counthr[loc], 
                   (int) (0.5 + 100.*icehr[loc]), 
                   fromlow[loc], navyhr[loc], rtghr[loc], delhr[loc], critflop);
      }
    }
    else {
      delhr[loc] = rgmin;
    }

    delrtg[loc] = rtghr[loc] - fromlow[loc];
    if (fabs(delrtg[loc]) > toler) {
        ll = delrtg.locate(loc);
        critflop = false;
        if ( (fromlow[loc] > tcrit && rtghr[loc] < tcrit) ||
             (fromlow[loc] < tcrit && rtghr[loc] > tcrit)  ) critflop = true;
        if (! (iceland[loc] == 157 && rtgland[loc] == 157 && newland[loc] == 2) ) {
          // don't print out if everyone agrees it is land
          printf("hirtg %6.2f %6.2f  %3d   %3d  %6.2f %6.2f %6.2f  %7.3f %1d\n",
                   ll.lat, ll.lon, counthr[loc], 
                   (int) (0.5 + 100.*icehr[loc]), 
                   fromlow[loc], navyhr[loc], rtghr[loc], delrtg[loc], critflop);
        }
      }

  }
  }

  //printf("delnav max, min, avg, rms %f %f %f %f\n",delhr.gridmax(rgmin), 
  //           delhr.gridmin(rgmin), delhr.average(rgmin), delhr.rms(rgmin) );
  //printf("delrtg max, min, avg, rms %f %f %f %f\n",delrtg.gridmax(rgmin), 
  //           delrtg.gridmin(rgmin), delrtg.average(rgmin), delrtg.rms(rgmin) );

///// Skip these, leave for later, probably separate program
//----------------------- Sea Ice-related checks:
  float tcold = 273.15 - 1.0;
  return 0;
  for (loc.j = 0; loc.j < delhr.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < delhr.xpoints(); loc.i++) {
    if (icehr[loc] != 0) {
      if (navyhr[loc] > tcrit || rtghr[loc] > tcrit || fromlow[loc] > tcrit) {
        // analysis too warm for ice being present
        if (! (icelandhr[loc] == 157 && rtglandhr[loc] == 157 && newland[loc] == 2) ) {
          // don't print out if everyone agrees it is land
          ll = icehr.locate(loc);
          printf("icew  %6.2f %6.2f  %3d   %3d  %6.2f %6.2f %6.2f  %7.3f\n",
                   ll.lat, ll.lon, counthr[loc], 
                   (int) (0.5 + 100.*icehr[loc]), 
                   fromlow[loc], navyhr[loc], rtghr[loc], delrtg[loc]);
        }
      }
    }
    if (icehr[loc] == 0 && icelandhr[loc] != 157 ) {
      if ((navyhr[loc] < tcold && navyhr[loc] > rgmin) || 
            rtghr[loc] < tcold || fromlow[loc] < tcold) {
        // analysis is cold for there being no ice present
        if (! (icelandhr[loc] == 157 && rtglandhr[loc] == 157 && newland[loc] == 2) ) {
          // don't print out if everyone agrees it is land
          ll = icehr.locate(loc);
          printf("icec  %6.2f %6.2f  %3d   %3d  %6.2f %6.2f %6.2f  %7.3f\n",
                   ll.lat, ll.lon, counthr[loc], 
                   (int) (0.5 + 100.*icehr[loc]), 
                   fromlow[loc], navyhr[loc], rtghr[loc], delrtg[loc]);
        }
      }
    }
  }
  }

  return 0;
}
void compare(llgrid<float> &x, llgrid<float> &y, char *tag) {
  llgrid<float> local(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
  ijpt loc, loc2;
  latpt ll;
  float delta;
  float toler = 1.5, rgmin = 265.0;

  if (fabs(x.dlat) < fabs(y.dlat)) {
    // x is higher resolution, repeat low res y onto local for comparisons
    for (loc.j = 0; loc.j < local.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < local.xpoints(); loc.i++) {
      ll = local.locate(loc);
      loc2 = y.locate(ll);
      local[loc] = y[loc2];
    }
    }
  }
  else if (fabs(x.dlat) > fabs(y.dlat)) {
    // x is lower resolution, average high res y on to local for comparisons
    llgrid<int> count(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
    count.set(0);
    local.set((float) 0.0);
    for (loc.j = 0; loc.j < local.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < local.xpoints(); loc.i++) {
      ll = local.locate(loc);
      loc2 = y.locate(ll);
      local[loc] += y[loc2];
      count[loc] += 1;
    }
    }
    for (loc.j = 0; loc.j < local.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < local.xpoints(); loc.i++) {
      if (count[loc] == 0) {
        local[loc] = rgmin;
      }
      else {
        local[loc] /= (float) count[loc];
      }
    }
    }
      
  }
  else {
    // x and y are same resolution, local = y
    local = y;
  }
 
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
      delta = (x[loc] - local[loc]);
      if (fabs(delta) > toler) {
        ll = x.locate(loc);
        printf("%s %6.2f %6.2f   %6.2f %6.2f %6.2f\n",
               tag,    ll.lat, ll.lon, 
                   x[loc], local[loc], delta);
      }
  }
  }

  return;
}
void compare(llgrid<float> &x, llgrid<unsigned char> &xland, llgrid<float> &y, char *tag) {
  llgrid<float> local(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
  ijpt loc, loc2;
  latpt ll;
  float delta;
  float toler = 1.5, rgmin = 265.0;

  //printf("dlats are %f %f %f\n",x.dlat, y.dlat, local.dlat);
  if (fabs(x.dlat) < fabs(y.dlat)) {
    // x is higher resolution, repeat low res y onto local for comparisons
    for (loc.j = 0; loc.j < local.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < local.xpoints(); loc.i++) {
      ll = local.locate(loc);
      loc2 = y.locate(ll);
      local[loc] = y[loc2];
    }
    }
    //printf("done transferring to higher resolution grid\n");
  }
  else if (fabs(x.dlat) > fabs(y.dlat)) {
    // x is lower resolution, average high res y on to local for comparisons
    llgrid<int> count(x.xpoints(), x.ypoints(), x.dlat, x.dlon, x.firstlat, x.firstlon);
    count.set(0);
    local.set((float) 0.0);
    for (loc.j = 0; loc.j < local.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < local.xpoints(); loc.i++) {
      ll = local.locate(loc);
      loc2 = y.locate(ll);
      local[loc] += y[loc2];
      count[loc] += 1;
    }
    }
    for (loc.j = 0; loc.j < local.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < local.xpoints(); loc.i++) {
      if (count[loc] == 0) {
        local[loc] = rgmin;
      }
      else {
        local[loc] /= (float) count[loc];
      }
    }
    }
      
  }
  else {
    // x and y are same resolution, local = y
    local = y;
  }
 
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
      delta = (x[loc] - local[loc]);
      if ((fabs(delta) > toler) && (xland[loc] == 0)) {
        ll = x.locate(loc);
        printf("%s %6.2f %6.2f   %6.2f %6.2f %6.2f\n",
               tag,    ll.lat, ll.lon, 
                   x[loc], local[loc], delta);
      }
  }
  }

  return;
}
