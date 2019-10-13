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

// 22 January 2009 
//   Read in 'accumulation' file and examine the net freeze/melt, and/or heating


int main(int argc, char *argv[]) {
// Inputs:
  gaussian<float> netmelt, netfreez, netheat, sum;

  ijpt xij;
  float count = 0.0, hrs = 3.0;
  float rho = 1000., cp = 4.e3, lf = 3.32e5, hmix = 25.0;
  FILE *metin, *fout;
  latpt ll;
  float hmin = 1.25, sstmin = 40.0;

  metin = fopen(argv[1],"r");
  if (metin == (FILE *) NULL) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }
  //printf("grid size: %d by %d\n",lh.xpoints(), lh.ypoints() );
  netmelt.binin(metin);
  netfreez.binin(metin);
  netheat.binin(metin);
  fclose(metin);

  sum = netmelt;
  sum += netfreez; 
  sum *= -1; // melt corresponds to + flux from atm in prior convention
  fout = fopen("net.freeze","w");
  sum.binout(fout);
  fclose(fout);
  fout = fopen("net.heating","w");
  netheat.binout(fout);
  fclose(fout);

  for (xij.j = 0; xij.j < netmelt.ypoints() ; xij.j++) {
  for (xij.i = 0; xij.i < netmelt.xpoints() ; xij.i++) {
    ll = sum.locate(xij);
    ll.lat = -ll.lat;
    // flip lat as the gaussian grid of this vintage doesn't have
    // current (24 Jun 2009) sign convention
    printf("%7.3f %7.3f  %6.2f %6.2f %6.2f  %7.2f\n",ll.lat, ll.lon, 
             netmelt[xij], netfreez[xij], sum[xij], netheat[xij] );
    if (fabs(sum[xij]) > hmin || fabs(netheat[xij]) > sstmin) {
      if (sum[xij] > hmin) sum[xij] = hmin;
      if (sum[xij] < -hmin) sum[xij] = -hmin;
      if (netheat[xij] > sstmin) netheat[xij] = sstmin;
      if (netheat[xij] < -sstmin) netheat[xij] = -sstmin;
    }
  }
  }

  palette<unsigned char> gg(19, 65);
  sum.scale();
  sum.xpm("sum.xpm",7,gg);
  netheat.scale();
  netheat.xpm("heat.xpm",7,gg);

  return 0;

}
