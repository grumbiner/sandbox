#include "ncepgrids.h"

#include "readers.C"
int ae(llgrid<float> &conc, grid2<unsigned char> &land, 
    float &nharea, float &nhextent, float &sharea, float &shextent) ;

int main(int argc, char *argv[]) {
  FILE *fin;

// NCEP analysis
  global_12th<float> ncep_conc;
  global_12th<unsigned char> land;
// IMS analysis
  bedient_north<float> ims_conc(96);
// RTOFS CICE
// GOFS3.1/ACNFS
// NSIDC

  fin = fopen(argv[1],"r");
  reader(ncep_conc, fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  reader(land, fin);
  fclose(fin);

  fin = fopen(argv[3], "r");
  reader(ims_conc, fin);
  fclose(fin);

// if both or neither have ice, zero the ncep
  ijpt loc;
  latpt ll;
  fijpt floc;
  for (loc.j = 0; loc.j < ncep_conc.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < ncep_conc.xpoints(); loc.i++) {
    ll = ncep_conc.locate(loc);
    if (ll.lat < 0) continue;
    floc = ims_conc.locate(ll);
    if (ims_conc.in(floc)) {
      if (ims_conc[floc] != 0. && ncep_conc[loc] != 0.) ncep_conc[loc] = 0.;
    }
  }
  }

  float nharea, nhextent, sharea, shextent;
  ae(ncep_conc, land,  nharea, nhextent, sharea, shextent);
  printf("IIEE NH %f %f SH %f %f Global %f %f \n", nharea, nhextent, 
   sharea, shextent, (nharea + sharea), (nhextent+shextent) );


  return 0;
}
int ae(llgrid<float> &conc, grid2<unsigned char> &land, float &nharea, float &nhextent, float &sharea, float &shextent) {
  ijpt loc;
  latpt ll;
  double area_sum_nh = 0.0, extent_sum_nh = 0.0;
  double area_sum_sh = 0.0, extent_sum_sh = 0.0;
 
  for (loc.j = 0; loc.j < conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < conc.xpoints(); loc.i++) {
    if (conc[loc] == 0. || land[loc] != 0) continue; // no ice or on land
    ll = conc.locate(loc);
    if (ll.lat > 0.) {
      area_sum_nh += conc.cellarea(loc)*conc[loc];
      extent_sum_nh += conc.cellarea(loc);
    }
    else {
      area_sum_sh += conc.cellarea(loc)*conc[loc];
      extent_sum_sh += conc.cellarea(loc);
    }
  }
  }
  nharea = area_sum_nh / 1.e12; // million km^2
  nhextent = extent_sum_nh / 1.e12;
  sharea = area_sum_sh / 1.e12; 
  shextent = extent_sum_sh / 1.e12;

  return 0;
}
