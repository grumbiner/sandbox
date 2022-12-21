#include "ncepgrids.h"

void reduce(global_12th<float> &hires, llgrid<float> &conc, int ratio, global_12th<unsigned char> &skip) {
  ijpt loc, lowloc;
  llgrid<int> *count;
  latpt ll;

  count = new llgrid<int>(conc.xpoints(), conc.ypoints(), conc.dlat, conc.dlon, 
                            conc.firstlat, conc.firstlon);
  
  count->set(0);
  conc.set((float) 0.0);

  //printf("conc.xpoints, ypoints = %d %d ratio = %d\n",
  //  conc.xpoints(), conc.ypoints(), ratio); fflush(stdout);
  for (loc.j = 0; loc.j < hires.ypoints(); loc.j++) {
    lowloc.j = loc.j / ratio;
  for (loc.i = 0; loc.i < hires.xpoints(); loc.i++) {
    lowloc.i = loc.i / ratio;
    
    if (skip[loc] == 0) {
      conc[lowloc] += hires[loc]; 
      count->operator[](lowloc) += 1;
    }
    if (skip[loc] != 0 && hires[loc] != 0) {
      //ll = skip.locate(loc);
      //printf("unskip %4d %4d  %1d %7.2f %7.2f  %5.2f\n",loc.i, loc.j, skip[loc], ll.lat, ll.lon, hires[loc]);
      hires[loc] = 0.0; // insurance against failures in preskip
    }
  }
  }

  for (loc.j = 0; loc.j < conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < conc.xpoints(); loc.i++) {
    if (count->operator[](loc) != 0) conc[loc] /= count->operator[](loc);
    if (conc[loc] < 0.15) conc[loc] = 0.;
  }
  }
  //printf("count max min avg %d %d %d\n",count->gridmax(), count->gridmin(), count->average() );
  //printf("conc max min avg %f %f %f\n",conc.gridmax(), conc.gridmin(), conc.average() );
  //printf("hires max min avg %f %f %f\n",hires.gridmax(), hires.gridmin(), hires.average() );


  return;
}
