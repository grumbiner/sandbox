#include <stdlib.h>
#include "ncepgrids.h"
#include "resops.h"

///////////// Utilities
// Compute a bathymetry for the target grid
void bathyfigure(char *fname, TARGET<float> &bathy,
                 TARGET<short int> &countp, TARGET<short int> &countm,
                 TARGET<int> &sump, TARGET<int> &summ);

///////////////////////////////////////////////////////////////////

int main(int argc, char *argv[]) {

  FILE *fin, *fout;

  TARGET<unsigned char> field;
  TARGET<float> bathy;
  TARGET<short int> countp, countm;
  TARGET<int> sump, summ; 

  latpt llat;
  ijpt loc;
  unsigned char coast = 0;
  unsigned char boundary = 1;
  unsigned char land = 5;
  unsigned char water = 17;
  unsigned char ocean = 15;
  unsigned char undef = 3;
  palette<unsigned char> gg(19, 65);
  int changed = 0, totpts = 0, vicinity = 1, count = 0;
  float minval, maxval; 

// Play with color table:
  gg.lighten(5, .5);
  gg.lighten(15, .5);

////////////////////////////////////////
  field.set( (unsigned char) undef);
  fin = fopen(argv[1],"r");
  field.binin(fin);
  fclose(fin);

// Get the bathymetry onto the target grid:
  bathyfigure(argv[2], bathy, countp, countm, sump, summ);

////////////////////////////
  #ifdef VERBOSE
    {
      latpt lat_ll, lat_ur;
      ijpt ll, ur;
      lat_ll.lat =   90.0; lat_ur.lat = -50.0;
      lat_ll.lon = -100.0; lat_ur.lon = -1.;
      ll = field.locate(lat_ll);
      ur = field.locate(lat_ur);
      printf("%d %d  %d %d\n",ll.i, ll.j, ur.i, ur.j);
    }
  #endif
// First, sweep through all the bountary points and make decisions for those
//   which are in the vicinity of ocean
  printf("about to sweeep through field\n"); fflush(stdout);
  while (vicinity > 0 && count < 50) {
    vicinity = 0;
    for (loc.j = 0; loc.j < field.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < field.xpoints(); loc.i++) {
      if (field[loc] == boundary ) {
        if ( field.anyof(ocean, 1, loc) > 0 ) {
          vicinity += 1;
          if (bathy[loc] > 0) {
            field[loc] = land;
          }
          else {
            field[loc] = ocean;
          }
        }
      }
    }
    }
    printf("pass %d bndy  points in vicinity of ocean %d\n",count, vicinity);
    count += 1;
  }


  for (loc.j = 0; loc.j < field.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < field.xpoints(); loc.i++) {
    totpts += 1;

    if (field[loc] == ocean) {
      if (bathy[loc] > 0 ) {
        if (countm[loc] < countp[loc] ) {
          llat = field.locate(loc);
        }
        else {  // will only be here for countm == countp
          bathy[loc] = (float) summ[loc] / (float) countm[loc] ;
          changed += 1;
        }
      }
        
    }
    
    else if (field[loc] == land && countm[loc] > countp[loc] 
              && (field.anyof(ocean, 1, loc) > 0)  ) {
      llat = field.locate(loc);
    }

  }
  }


  printf("number of points changed = %d of %d\n",changed, totpts); 

  // Now apply the coastal flag
  minval = atof(argv[5]);
  maxval = atof(argv[6]);
  totpts = 0;
  for (loc.j = 0; loc.j < field.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < field.xpoints(); loc.i++) {
     if (bathy[loc] > minval && bathy[loc] < maxval ) {
          //&& field.anyof(ocean,30,loc) > 0 ) {
       totpts += 1;
       field[loc] = coast;
     }
  }
  }
  printf("Found %d points between %f and %f elevation\n",
               totpts, minval, maxval);

  fout = fopen(argv[3],"w");
  bathy.binout(fout);
  field.binout(fout);
  fclose(fout);

  field.xpm(argv[4], 1, gg);
  bathy.scale();
  bathy.xpm("bathymetry.xpm",7,gg);

  return 0;
}

//////////////////////////
// Given a bathymetry, construct a target grid bathymetry
void bathyfigure(char *fname, TARGET<float> &bathy,
                 TARGET<short int> &countp, TARGET<short int> &countm,
                 TARGET<int> &sump, TARGET<int> &summ) {
// Etopo2 input
  llgrid<short int> topo(10800, 5400, -1./30., 1./30., 90.0, -180.0);

  FILE *fin;
  ijpt loc, iloc;
  fijpt ij1;
  latpt llat;
  int pass = 0;

  fin = fopen(fname, "r");
  topo.binin(fin);
  fclose(fin);

  //printf("Have entered bathyfigure and read in the topography\n");
  //fflush(stdout);

  summ.set(0);
  sump.set(0);
  countm.set(0);
  countp.set(0);
  bathy.set( (float) 0.);

  printf("topography max, min, average %d %d %d\n",topo.gridmax(), 
          topo.gridmin(), topo.average() ); fflush(stdout);
  if (topo.gridmin() == topo.gridmax() ) {
    printf("topo max and min are identical, quitting!\n");
    exit(2);
  }
  for (loc.j = 0; loc.j < topo.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < topo.xpoints(); loc.i++) {
    llat = topo.locate(loc);
    ij1  = bathy.locate(llat);
    iloc.i = (int) (0.5 + ij1.i);
    iloc.j = (int) (0.5 + ij1.j);
    if (bathy.in(ij1) ) {
      if (topo[loc] > 0) {
        sump[iloc] += topo[loc];
        countp[iloc] += 1;
      }
      else if (topo[loc] < 0) {
        summ[iloc] += topo[loc];
        countm[iloc] += 1;
      }
      else {
        // Policy:
        //printf("== 0 topo at %f %f\n",llat.lat, llat.lon); fflush(stdout);
        //unnecessary: summ[loc] += topo[loc];
        countm[iloc] += 1;
      }
    }
    pass += 1;
    fflush(stdout);
  }
  }

  // Now that we've dropped in bucket (policy) the topography to target
  //   grid, average and look at flags
  printf("Done with averaging process, max counts and sums are:\n");
  printf("Countp, countm %d %d summ sump %d %d\n",countp.gridmax(), countm.gridmax(),
              summ.gridmin(), sump.gridmax() );
  fflush(stdout);
  for (loc.j = 0; loc.j < summ.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < summ.xpoints(); loc.i++) {
    if (countm[loc] == 0 && countp[loc] == 0) {
      printf("Failed to find any valid topography information at %d %d!\n",
          loc.i, loc.j); fflush(stdout);
    }
    // Policies:
    else if (countm[loc] > countp[loc] && countm[loc] > 0) {
      bathy[loc] = (float) summ[loc] / (float) countm[loc];
    }
    else if (countm[loc] != countp[loc] ) {
      if (countp[loc] > 0) 
        bathy[loc] = (float) sump[loc] / (float) countp[loc];
    }
    else {  // In case of tie, use whichever is furthest from sea level policy
      if (-summ[loc] > sump[loc] ) {
        bathy[loc] = (float) summ[loc] / (float) countm[loc];
      }
      else {
        bathy[loc] = (float) sump[loc] / (float) countp[loc];
      }

    }      

  }
  }

  return;     
}
