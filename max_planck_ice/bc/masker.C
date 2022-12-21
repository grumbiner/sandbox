#include "ncepgrids.h"

// Convert land mask from satellite grid to ice model grid
// At this point, use (require) same grids for the two.
// Initial Translation Rules:
//      SSMI Grid                  Ice Model Grid
//   Point is land (flag = 157)   then land (flag = 0) 
//   Point is ocean (flag = 0)    then ocean(flag = 1)
//   Point is coast (flag=195)    then ocean(flag = 1)
// Mask fix:
//   The ice model chokes if there are ocean points with no adjacent water
//   points.  Therefore, pave over all isolated points.
// Bathymetry fixes:
//   We're assuming we like the land mask better than the bathymetry file.
//   Therefore, if a point is water and the bathymetry is less than 15 meters
//   deep, give it a 15 meter depth.  If a point is land, set bathymetry to 
//   1 meter depth.
// Robert Grumbine
// 26 July 2002

int main(int argc, char *argv[]) {
  GRIDTYPE<float> landref, bathy;
  GRIDTYPE<float> landout, bathyout;
  GRIDTYPE<float> neigh;
  FILE *fin, *fout;
  ijpt loc;
  latpt ll;
  int i;


  fin = fopen(argv[1], "r");
  landref.ftnin(fin);
  fclose(fin);
  landref *= 100;

  fin = fopen(argv[2], "r");
  bathy.ftnin(fin);
  fclose(fin);
  bathyout = bathy;

// Initial Translation Rules:
//      SSMI Grid                  Ice Model Grid
//   Point is land (flag = 157)   then land (flag = 0) 
//   Point is ocean (flag = 0)    then ocean(flag = 1)
//   Point is coast (flag=195)    then ocean(flag = 1)
  for (loc.j = 0; loc.j < landref.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < landref.xpoints(); loc.i++) {
    landref[loc] = (float) ( (int) landref[loc]);
    if (landref[loc] == 157) {
      landout[loc] = 0.;
    }
    else if (landref[loc] == 0 || landref[loc] == 195) {
      landout[loc] = 1.; 
    }
    else {
      printf("land mask flag out of range at %3d %3d, val = %f\n",
          loc.i, loc.j, landref[loc]);
      landout[loc] = 0;
    }
  }
  }

// Mask fix:
//   The ice model chokes if there are ocean points with no adjacent water
//   points.  Therefore, pave over all isolated points.
  //printf("about to compute neigh\n"); fflush(stdout);
  neigh.set(0);
  for (loc.j = 1; loc.j < landout.ypoints()-2; loc.j++) {
  for (loc.i = 1; loc.i < landout.xpoints()-2; loc.i++) {
    neigh[loc] += landout[loc.i-1 + loc.j*landout.xpoints() ] +
                  landout[loc.i+1 + loc.j*landout.xpoints() ] +
                  landout[loc.i   + (loc.j-1)*landout.xpoints() ] +
                  landout[loc.i   + (loc.j+1)*landout.xpoints() ] ;
  }
  }
  for (loc.j = 1; loc.j < landout.ypoints()-1; loc.j++) {
  for (loc.i = 1; loc.i < landout.xpoints()-1; loc.i++) {
    if (landout[loc] == 1 && neigh[loc] == 0) {
      landout[loc] = 0.;
    }
  }
  }
  //printf("done with neigh\n"); fflush(stdout);

// Bathymetry fixes:
//   We're assuming we like the land mask better than the bathymetry file.
//   Therefore, if a point is water and the bathymetry is less than 15 meters
//   deep, give it a 15 meter depth.  If a point is land, set bathymetry to 
//   1 meter depth.
  for (loc.j = 0; loc.j < landout.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < landout.xpoints(); loc.i++) {
    if (landout[loc] == 0. && bathy[loc] > 1.) bathyout[loc] = 1.;
    if (landout[loc] == 1. && bathy[loc] < 15.) bathyout[loc] = 15.;
  }
  }
  fout = fopen(argv[3], "w");
  bathyout.ftnout(fout);
  fclose(fout);

// Construct inner mask from neigh (recompute since we updated the masks 
//  above).
// If a point has 2, 3, 4 ocean point neighbors, then make it an ocean point.
// If it has 0 or 1, make land.
  neigh.set(0);
  for (loc.j = 1; loc.j < landout.ypoints()-2; loc.j++) {
  for (loc.i = 1; loc.i < landout.xpoints()-2; loc.i++) {
    neigh[loc] += landout[loc.i-1 + loc.j*landout.xpoints() ] +
                  landout[loc.i+1 + loc.j*landout.xpoints() ] +
                  landout[loc.i   + (loc.j-1)*landout.xpoints() ] +
                  landout[loc.i   + (loc.j+1)*landout.xpoints() ] ;
   if (neigh[loc] >= 2) {
     neigh[loc] = 1;
   }
   else {
     neigh[loc] = 0;
   }
  }
  }

// Now establish the boundary mask values
   //printf("about to run through perimeter points\n"); fflush(stdout);
   loc.i = 0; loc.j = 0;  landout[loc] = 0;
   loc.i = landout.xpoints()-1; landout[loc] = 0;
   for (loc.j = 1; loc.j < landout.ypoints(); loc.j++) { 
     landout[loc] = 0;
     neigh[loc] = 0;
   }
   loc.i = 0;
   for (loc.j = 1; loc.j < landout.ypoints(); loc.j++) {
     landout[loc] = 0;
   }
   loc.i = 1;
   for (loc.j = 1; loc.j < landout.ypoints(); loc.j++) {
     neigh[loc] = 0;
   }

//   printf("about to do the other sides of the perimeter\n"); fflush(stdout);
   loc.i = 0;  loc.j = landout.ypoints();
   landout[loc] = 0;
   loc.j = 0;
   for (loc.i = 1; loc.i < landout.xpoints(); loc.i++) {
     landout[loc] = 0;
   }
   loc.j = 1;
   for (loc.i = 1; loc.i < landout.xpoints(); loc.i++) {
     neigh[loc] = 0;
   }

   loc.j = landout.ypoints() - 1;
   for (loc.i = 1; loc.i < landout.xpoints(); loc.i++) {
     landout[loc] = 0;
     neigh[loc] = 0;
   }

// Print out for use by rest of system
//   printf("printing out data points\n"); fflush(stdout);
   for (loc.j = 1; loc.j < neigh.ypoints(); loc.j++) {
     for (loc.i = 1; loc.i < neigh.xpoints(); loc.i++) {
        printf("%1d",(int)neigh[loc]);
     }
     printf("\n");
   }
   for (loc.j = 0; loc.j < landout.ypoints(); loc.j++) {
     for (loc.i = 0; loc.i < landout.xpoints(); loc.i++) {
        printf("%1d",(int)landout[loc]);
     }
     printf("\n");
   }
   for (loc.j = 0; loc.j < landout.ypoints(); loc.j++) {
     for (loc.i = 0; loc.i < landout.xpoints(); loc.i++) {
        printf("%1d",(int)landout[loc]);
     }
     printf("\n");
   }


   fout = fopen(argv[4], "w");
   landout.ftnout(fout);
   fclose(fout);

   return 0;
}
