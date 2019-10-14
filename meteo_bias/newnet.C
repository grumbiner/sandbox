#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>

#include "ncepgrids.h"
#include "points.h"

// Compute the net fluxes from the mrf in terms of an equivalent
//   melt/freeze rate when ice is present, and in terms of heating
//   a 25 meter ocean mixed layer when it isn't.
// Robert Grumbine 5 February 1998

// Restart 16 January 2009
//   assume that input files are set up,
//   read through, and average them up for whatever period is
//   provided

int main(int argc, char *argv[]) {
// Inputs:
  gaussian<float> sh, lh, lwdown, swdown, lwup, swup;
  gaussian<float> icec;
  gaussian<float> gflux, land, icetk, snod, prate;

  gaussian<float> netrad, netsurf;
  gaussian<float> netmelt, netfreez, netaccum, netheat;

  ijpt xij;
  float count = 0.0, hrs = 3.0;
  float rho = 1000., cp = 4.e3, lf = 3.32e5, hmix = 25.0;
  FILE *metin, *fout;

  metin = fopen(argv[1],"r");
  if (metin == (FILE *) NULL) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }
  //printf("grid size: %d by %d\n",lh.xpoints(), lh.ypoints() );
  if (argc == 3) {
    fout = fopen(argv[2], "r+");
    if (fout == (FILE *) NULL) {
      printf("failed to open inout file %s\n",argv[2]);
      return 2;
    }
    netmelt.binin(fout);
    netfreez.binin(fout);
    netheat.binin(fout);
    rewind(fout);
  }
  else {
    netmelt.set(0.0);
    netfreez.set(0.0);
    netheat.set(0.0);
    fout = fopen("accumulation", "w");
    if (fout == (FILE *) NULL) {
      printf("failed to open out file accumulation\n");
      return 2;
    }
  }
  
  sh.set(0.0);
  lh.set(0.0);
  lwdown.set(0.0);
  lwup.set(0.0);
  swup.set(0.0);
  swdown.set(0.0);


  count = 0.0;
  while (!feof(metin) && count < 8* 14) {
  //while (!feof(metin) ) {

    sh.binin(metin);
    if (feof(metin)) continue;
    lh.binin(metin);
    if (feof(metin)) continue;
    lwdown.binin(metin);
    if (feof(metin)) continue;
    lwup.binin(metin);
    if (feof(metin)) continue;
    swup.binin(metin);
    if (feof(metin)) continue;
    swdown.binin(metin);
    if (feof(metin)) continue;
    icec.binin(metin);
    if (feof(metin)) continue;
    gflux.binin(metin);
    if (feof(metin)) continue;
    land.binin(metin);
    if (feof(metin)) continue;
    icetk.binin(metin);
    if (feof(metin)) continue;
    snod.binin(metin);
    if (feof(metin)) continue;
    prate.binin(metin);

    //printf("icec max min average %f %f %f\n",icec.gridmax(), icec.gridmin(), icec.average() );
    count += 1.0;

    netrad.set(0.0);
    netsurf.set(0.0);

// gflux seems to carry flag values
    //printf("gflux max, min %f %f\n",gflux.gridmax(), gflux.gridmin() );
    if (gflux.gridmax() < -1e6) {
      for (xij.j = 0; xij.j < netrad.ypoints() ; xij.j++) {
      for (xij.i = 0; xij.i < netrad.xpoints() ; xij.i++) {
        if (gflux[xij] < -1e6) gflux[xij] = 0;
      }
      }
    }
    if (gflux.gridmax() > 1e6) {
      for (xij.j = 0; xij.j < netrad.ypoints() ; xij.j++) {
      for (xij.i = 0; xij.i < netrad.xpoints() ; xij.i++) {
        if (gflux[xij] > 1e6) gflux[xij] = 0;
      }
      }
    }
      
    if (!feof(metin) ) {
      for (xij.j = 0; xij.j < netrad.ypoints() ; xij.j++) {
      for (xij.i = 0; xij.i < netrad.xpoints() ; xij.i++) {
// Verified vs. YY Chao Note 4/15/2009
// negative netsurf means surface cooling/freezing 
        netsurf[xij] = -sh[xij] - lh[xij] - gflux[xij] ;
        //netsurf[xij] = -sh[xij] - lh[xij];

        netrad[xij] = (lwdown[xij] + swdown[xij] - lwup[xij] - swup[xij]*0.9 );

        netsurf[xij] += netrad[xij];

        if (icec[xij] > 0 && netsurf[xij] > 0) { 
          netmelt[xij] += (netsurf[xij]*hrs*3600.) / (rho*lf) ;
        }
        //if (icec[xij] > 0 && netsurf[xij] < 0) {
        else {
          netfreez[xij] += (netsurf[xij]*hrs*3600.) / (rho*lf) ;
        } 

        if (icec[xij] == 0) {
          //netheat[xij] += netsurf[xij] / (rho*cp*hmix) * hrs * 3600.;
          netheat[xij] += (netsurf[xij] * hrs * 3600.) / (rho*cp*hmix) ;
        } 
      }
      }

      printf("%f netsurf average %f\n",count, netsurf.average() ); fflush(stdout);

    } // end of accumulating fluxes
  }


//  Now have integrated equivalent fluxes.  Melt in meters, heat in Joules
  netmelt.binout(fout);
  netfreez.binout(fout);
  netheat.binout(fout);
  fclose(fout);

  #ifdef VERBOSE
  latpt ll;
  for (xij.j = 0; xij.j < netrad.ypoints() ; xij.j++) {
  for (xij.i = 0; xij.i < netrad.xpoints() ; xij.i++) {
    ll = netrad.locate(xij);
    //if (ll.lat > 30.0 || ll.lat < -30.0) {
      //printf("%7.3f %7.3f  %6.2f %6.2f %6.2f  %7.2f   %8.2f\n", 
      printf("%7.3f %7.3f  %6.2f %6.2f %6.2f  %7.2f\n", 
           ll.lat, ll.lon,
           netmelt[xij], netfreez[xij], 
           netmelt[xij]+netfreez[xij],  netheat[xij]);
           //netmelt[xij]+netfreez[xij],  netheat[xij], netrad[xij]);
    //}
  }
  }
  #endif

  netaccum = netfreez;
  netaccum -= netmelt;
  palette<unsigned char> gg(19, 65);
  netheat.scale();
  netheat.xpm("heating.xpm",7,gg);

  netmelt.scale();
  netmelt.xpm("melting.xpm",7,gg);

  netfreez.scale();
  netfreez.xpm("freezing.xpm",7,gg);

  netaccum.scale();
  netaccum.xpm("accum.xpm",7,gg);

  return 0;

}
