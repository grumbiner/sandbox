#include <stdio.h>
#include <math.h>

//This is a sample program to construct grib messages using the
//  C++ class library.  
//Sample grids are lat-long (chalgrid) and polar stereographic (northgrid).
//Robert Grumbine 7 May 1998.

#include "ncepgrids.h"

int main(void) {
  global_ice<float> field;     // Test with global ice lat-long grid
  northgrid<float> icefield;   // Test with NHem. sea ice (polar stereo)
  char *grib;
  int lgrib=0, ierr=0;
  FILE *fout;

  // The following are parameters that you'll be setting in your program
  int parmno=91, depth=0, lead=0, mxbit=8;

  // And this is a utility variable I use to pick a point (i,j location) to set
  // to a non-zero value 
  ijpt loc;

// Set up data field:
  field.set(0.);
  icefield.set(0.);
  loc.i = 23;
  loc.j = 5;
  field[loc] = 23.;
  icefield[loc] = 23.;

// Set up pds:
  field.pds.set_time(1998, 5, 6, 12, 30);
  field.pds.set_precision(1.);
  icefield.pds.set_time(1998, 5, 6, 12, 30);
  icefield.pds.set_precision(1.);

// Open a file for output
    fout = fopen("testout", "w");
    if (fout == NULL) {
      printf("Failed to open the output file\n");
    }

// Call the gribit routine for a lat-long grid:
    grib = new char [(field.xpoints() * field.ypoints() * mxbit)/8 + 200];
    if (grib == NULL) {
      printf("failed to new the grib field\n");
    }

    ierr = field.gribit(parmno, depth, lead, grib, lgrib, mxbit);
    if (ierr != 0) {
      printf("Error %d in constructing grib file!\n",ierr);
    } 
    else {
      fwrite(grib, sizeof(char), lgrib, fout); 
    }

// Call the gribit routine for a lat-long grid:
    // Release the old grib message, and then make space for the new one
    delete grib;
    grib = new char [(icefield.xpoints() * icefield.ypoints() *
                                  mxbit)/8 + 200];
    if (grib == NULL) {
      printf("failed to new the grib icefield\n");
    }

    icefield.pds.set_layer(102);  //Note that we're resetting the layer type
    ierr = icefield.gribit(parmno, depth, lead, grib, lgrib, mxbit);

    // Check that we ran ok, and if so, write out the grib message
    if (ierr != 0) {
      printf("Error %d in constructing grib file!\n",ierr);
    } 
    else {
      fwrite(grib, sizeof(char), lgrib, fout); 
    }
  
// Check Y2K
  printf("Will now check the Y2K compliance\n");
  field.pds.set_time(1999, 5, 6, 12, 30);
  field.pds.show_date();
  field.pds.set_time(2000, 5, 6, 12, 30);
  field.pds.show_date();
  field.pds.set_time(2001, 5, 6, 12, 30);
  field.pds.show_date();
  field.pds.set_time(2099, 5, 6, 12, 30);
  field.pds.show_date();
  field.pds.set_time(2100, 5, 6, 12, 30);
  field.pds.show_date();
  field.pds.set_time(2101, 5, 6, 12, 30);
  field.pds.show_date();
  return 0;
}
