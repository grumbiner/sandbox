#include "ncepgrids.h"

// Produce some simple graphics from simple sea ice model's output
//  Variant for gfs fakery

int main(int argc, char *argv[]) {
  gaussian<float> ice_conc, ice_thick, mask;

  FILE *fin, *fout;
  char fname[90];
  palette<unsigned char> gg(19, 65);
  global_quarter<float> concout, thickout;
  float landval = 999., nonval = -0.1;
  int nstep, nhour;
  ijpt loc;
  latpt ll;

  fin = fopen(argv[1],"r");
  fout = fopen(argv[2],"w");
  if (fout == (FILE *) NULL ) {
    printf("Failed to open the output file! %s \n",argv[3]); fflush(stdout);
  }
  nstep = atoi(argv[3]);
  nhour = atoi(argv[4]);

  for (int i = 1; i <= nstep; i++) {
    ice_conc.binin(fin);
    ice_thick.binin(fin);

    // cap at my landval to avoid issues with the e20 values
    for (loc.j = 0; loc.j < ice_conc.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < ice_conc.xpoints(); loc.i++) {
      if (ice_conc[loc] > landval || ice_thick[loc] > landval ) {
        ice_conc[loc]  = landval;
        ice_thick[loc] = landval;
        mask[loc]      = landval;
      }
      else {
        mask[loc] = 0.0;
      }
    }
    }

    //debug printf("input conc thick max min %f %f  %f %f\n",ice_conc.gridmax(), 
    //debug                ice_conc.gridmin(), ice_thick.gridmax(), ice_thick.gridmin() );

     concout.fromall(ice_conc , mask, landval, nonval);
    thickout.fromall(ice_thick, mask, landval, nonval);
    for (loc.j = 0; loc.j < concout.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < concout.xpoints(); loc.i++) {
      ll = concout.locate(loc);
      if (ll.lat < -89.8) { 
        // do this because probably some error in revinterp for gaussian
        concout[loc]  = nonval;
        thickout[loc] = nonval;
      }
      if (fabs(concout[loc]) > landval) {
        printf("c %d %d  %f %f  %f\n",loc.i, loc.j, ll.lat, ll.lon, concout[loc]);
        concout[loc] = nonval;
      }
      if (fabs(thickout[loc]) > landval) {
        printf("t %d %d  %f %f  %f\n",loc.i, loc.j, ll.lat, ll.lon, thickout[loc]);
        thickout[loc] = nonval;
      }
    }
    }


  //debug printf("interp conc thick max min %f %f  %f %f\n",concout.gridmax(), 
         //debug          concout.gridmin(), thickout.gridmax(), thickout.gridmin() );

  //debug printf("%d max, min ice concentrations %f %f\n",i, ice_conc.gridmax(), 
         //debug           ice_conc.gridmin() );
  //debug printf("%d max, min ice thick %f %f\n",i, ice_thick.gridmax(), 
         //debug          ice_thick.gridmin() );

    concout.binout(fout);
    thickout.binout(fout);
    // as global_12th, floating point (4byte) nos., this over-runs file 
    //                    size limits @28th step.
    // using global_quarter.

    
    for (loc.j = 0; loc.j < ice_conc.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < ice_conc.xpoints(); loc.i++) {
       if (ice_conc[loc] == landval || ice_thick[loc] == nonval) {
         ice_conc[loc] = 0;
       }
       if (ice_thick[loc] == landval || ice_thick[loc] == nonval) {
         ice_thick[loc] = 0;
       }
    }
    }
    ice_conc *= 100.0;
    sprintf(fname,"conc%03d.xpm",i*nhour);
    ice_conc.xpm(fname, 7, gg);

    ice_thick /= 0.10;
    sprintf(fname,"thick%03d.xpm",i*nhour);
    ice_thick.xpm(fname, 1, gg);

  }

  fclose(fout);

  return 0;
}
