#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin_n, *fin_s, *fout_n, *fout_s, *fnland, *fsland;
  northgrid<unsigned char> nland;
  southgrid<unsigned char> sland;
  northgrid<float> nconc, nmask, nout;
  southgrid<float> sconc, smask, sout;
  float maskval = 2.24;
  ijpt loc;

  fin_n = fopen(argv[1],"r");
  fin_s = fopen(argv[2], "r");
  fout_n = fopen(argv[3], "w");
  fout_s = fopen(argv[4], "w");
  fnland = fopen(argv[5], "r");
  fsland = fopen(argv[6], "r");
  if (fin_n == NULL || fin_s == NULL || fout_n == NULL || fout_s == NULL ||
      fnland == NULL || fsland == NULL ) {
    printf("Failed to open a required file\n");
    if (fin_n == NULL ) printf(" -- input north\n");
    if (fin_s == NULL ) printf(" -- input south\n");
    if (fout_n == NULL ) printf(" -- output north\n");
    if (fout_s == NULL ) printf(" -- output south\n");
    if (fnland == NULL ) printf(" -- north land mask\n");
    if (fsland == NULL ) printf(" -- south land mask\n");
    return -1;
  }
  nland.binin(fnland);
  sland.binin(fsland);
  nconc.binin(fin_n);
  sconc.binin(fin_s);
  printf("North max, min %f %f\n", nconc.gridmax(), nconc.gridmin() );
  printf("South max, min %f %f\n", sconc.gridmax(), sconc.gridmin() );
  printf("Northland max, min %f %f\n", (float) nland.gridmax(), (float) nland.gridmin() );
  printf("Southland max, min %f %f\n", (float) sland.gridmax(), (float) sland.gridmin() );
  #ifdef VERBOSE
    nconc.printer(stdout);
    sconc.printer(stdout);
  #endif

  nmask.set((float)0.0);
  smask.set((float)0.0);

  for (loc.j = 0; loc.j < nconc.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < nconc.xpoints() ; loc.i++) {
    if (nconc[loc] > 1.28 || (int) nland[loc] >= 1 ) {
       nmask[loc] = maskval; 
       nconc[loc] = 0.0;
    }
  }
  }
  for (loc.j = 0; loc.j < sconc.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < sconc.xpoints() ; loc.i++) {
    if (sconc[loc] > 1.28 || (int) sland[loc] >= 1 ) {
       smask[loc] = maskval; 
       sconc[loc] = 0.0;
    }
  }
  }

  nout.reduce(nconc, nmask, maskval);
  sout.reduce(sconc, smask, maskval);

  for (loc.j = 0; loc.j < nout.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < nout.xpoints() ; loc.i++) {
    if (nout[loc] < 0.15) nout[loc] = 0.0;
    if (nout[loc] > 1.0 && nout[loc] < 1.28) nout[loc] = 1.0;
  }
  }
  for (loc.j = 0; loc.j < sout.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < sout.xpoints() ; loc.i++) {
    if (sout[loc] < 0.15) sout[loc] = 0.0;
    if (sout[loc] > 1.0 && sout[loc] < 1.28) sout[loc] = 1.0;
  }
  }

  printf("Reduced North max, min %f %f\n", nout.gridmax(), nout.gridmin() );
  printf("Reduced South max, min %f %f\n", sout.gridmax(), sout.gridmin() );
  printf("NH area now %6.3f orig %6.3f\n",nout.integrate()/1e12,
                                          nconc.integrate()/1.e12);
  printf("SH area now %6.3f orig %6.3f\n",sout.integrate()/1e12,
                                          sconc.integrate()/1.e12);
  #ifdef VERBOSE
    printf("Values of nout, sout from reduce\n");
    nout.printer(stdout);
    sout.printer(stdout);
  #endif  

  nout.binout(fout_n);
  sout.binout(fout_s);

  return 0;

}
