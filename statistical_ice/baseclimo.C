#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northgrid<float> ice;
  northgrid<unsigned char> land;
  global_sst<float> sst;
  FILE *icein, *sstin, *landin;
  float sstfilt = 275.15;
  ijpt iloc, sloc;
  latpt ll;

  icein = fopen(argv[1],"r");
  ice.binin(icein);
  fclose(icein);
  if (ice.gridmax() >= 100.) ice /= 100.;

  sstin = fopen(argv[2],"r");
  sst.binin(sstin);
  fclose(sstin);

  landin = fopen(argv[3],"r");
  land.binin(landin);
  fclose(landin);

  for (iloc.j = 0; iloc.j < ice.ypoints(); iloc.j++) {
  for (iloc.i = 0; iloc.i < ice.xpoints(); iloc.i++) {
    ll = ice.locate(iloc);
    sloc = sst.locate(ll);
    // filter for warm sst or for being land/coast
    if (sst[sloc] >= sstfilt || land[iloc] > 100 ) ice[iloc] = 0.0;
    // bound ice concentrations in case they aren't already
    if (ice[iloc] > 1.00 && ice[iloc] < 1.5) ice[iloc] = 1.00;
  }
  }

  icein = fopen(argv[4],"w");
  ice.binout(icein);
  fclose(icein);
  printf("%s max min avg %f %f %f\n",argv[1],ice.gridmax(), ice.gridmin(), ice.average() );

  return 0;
}
