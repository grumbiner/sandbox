#include "ncepgrids.h"

#define MAXICE 127
#define MINICE 15

int main(int argc, char *argv[]) {
  global_ice<unsigned char> land;
  global_ice<float> ice, n, s;
  unsigned char nonval;

  ijpt loc;
  latpt ll;
  FILE *fin;

  fin = fopen(argv[1], "r");
  land.binin(fin);
  fclose(fin);

  fin = fopen(argv[2], "r");
  ice.binin(fin);
  fclose(fin);
  if (ice.gridmax() < 3.0) ice *= 100.;

  nonval = ice.gridmax();
  if (nonval <= MAXICE) nonval = 224;
  
// Mask out land and input nonvalues of all types 
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (ice[loc] > MAXICE) ice[loc] = nonval;
    if (ice[loc] < MINICE) ice[loc] = 0;
    if (ice[loc] > 100 && ice[loc] <= MAXICE) ice[loc] = 100;  

    if (land[loc] > 0 ) {
      ice[loc] = nonval;
    }

  }
  }

  n = ice;
  s = ice;
  
  //mask out the other hemisphere for doing integrations
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
    loc.i = 0;
    ll = n.locate(loc);
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (ll.lat <= 0) n[loc] = 0;
    if (ll.lat >= 0) s[loc] = 0;
  }
  }
 
  printf("area %9.4f %9.4f %9.4f ",(float) ice.integrate(nonval)/1.e12 /100., 
             (float) n.integrate(nonval)/1.e12 /100.,
             (float) s.integrate(nonval)/1.e12 /100.  );

  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (ice[loc] > MINICE && ice[loc] <= MAXICE) {
      ice[loc] = 1;
    }
    else {ice[loc] = 0; }
    if (n[loc] > MINICE && n[loc] <= MAXICE) n[loc] = 1;
      else {n[loc] = 0; }
    if (s[loc] > MINICE && s[loc] <= MAXICE) s[loc] = 1;
      else {s[loc] = 0; }
  }
  }

  printf("extent %9.4f %9.4f %9.4f\n",(float) ice.integrate(nonval)/1.e12 , 
             (float) n.integrate(nonval)/1.e12,
             (float) s.integrate(nonval)/1.e12   );


  return 0;
}
