#include "ncepgrids.h"

// Analyze metfile contents 

void checker(metricgrid<float> &h, char *field, char *param, ijpt &sp) ;

int main(int argc, char *argv[]) {
  northgrid<float> tair, slp, rh, tsfc, swdn, lwdn, lwup, rprec, mask;
  psgrid<float> uwin, vwin;
  char field[900], parameter[900];

  FILE *fin;
  int i;
  ijpt sploc;
  
  latpt ll;
  ll.lon = 0;
  ll.lat = 90;
  sploc = swdn.locate(ll); 

  fin = fopen (argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open input file %s\n", argv[1]);
    return 1;
  }
  tair.ftnin(fin);
  sprintf(field, "Air Temperature");
  sprintf(parameter, "tair");
  checker(tair, field, parameter, sploc);

  slp.ftnin(fin);
  sprintf(field, "Sea Level Pressure");
  sprintf(parameter, "slp");
  checker(slp, field, parameter, sploc);

  rh.ftnin(fin);
  sprintf(field, "Relative humidity");
  sprintf(parameter, "rh");
  checker(rh, field, parameter, sploc);

  tsfc.ftnin(fin);
  sprintf(field, "Surface Temperature");
  sprintf(parameter, "tsfc");
  checker(tsfc, field, parameter, sploc);

  swdn.ftnin(fin);
  sprintf(field, "Downwelling Shortwave");
  sprintf(parameter, "swdn");
  checker(swdn, field, parameter, sploc);

  lwdn.ftnin(fin);
  sprintf(field, "Downwelling longwave");
  sprintf(parameter, "lwdn");
  checker(lwdn, field, parameter, sploc);

  lwup.ftnin(fin);
  sprintf(field, "Upwelling longwave");
  sprintf(parameter, "lwup");
  checker(lwup, field, parameter, sploc);

  rprec.ftnin(fin);
  sprintf(field, "rprec");
  sprintf(parameter, "rprec");
  checker(rprec, field, parameter, sploc);

  mask.ftnin(fin);
  sprintf(field, "Mask");
  sprintf(parameter, "mask");
  checker(mask, field, parameter, sploc);

//  uwin.ftnin(fin);
//  sprintf(field, "Uwin");
//  sprintf(parameter, "Uwin");
//  checker(uwin, field, parameter, sploc);
//
//  vwin.ftnin(fin);
//  sprintf(field, "Vwin");
//  sprintf(parameter, "Vwin");
//  checker(vwin, field, parameter, sploc);


  return 0;
} 

void checker(metricgrid<float> &h, char *field, char *param, ijpt &sp) {
  ijpt loc;
  latpt ll;

  printf("%s\n",field);
  printf("max, min, average, rms %f %f %f %f  %f\n",h.gridmax(), h.gridmin(),
      h.average(0.), h.rms(0.), h[sp]  );
  for (loc.j = 0; loc.j < h.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < h.xpoints(); loc.i++) {
     if (! finite(h[loc]) ) {
       ll = h.locate(loc);
       printf("Found nan in %s at %d %d %f %f  %f\n",param, loc.i, loc.j, 
             ll.lat, ll.lon, h[loc]);
     }
  }
  }

} 
