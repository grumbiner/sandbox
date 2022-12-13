#include "ncepgrids.h"

#define MAXICE 127
#define MINICE 15

#define count 0
#define avg   1
#define sumsq 2
#define var   3
#define CONDAVG  4
#define CONSUMSQ 5
#define CONDVAR  6


int main(int argc, char *argv[]) {
  global_ice<unsigned char> land;
  global_ice<float> ice[7], n, s, glob;
  unsigned char nonval;

  ijpt loc;
  latpt ll;
  FILE *fin;
  int i, nreq;

  fin = fopen(argv[1], "r");
  land.binin(fin);
  fclose(fin);

  fin = fopen(argv[2], "r");
  for (i = 0; i < 7; i++) {
    ice[i].binin(fin);
  }
  fclose(fin);

  #ifdef CONDITIONAL
    if (ice[CONDAVG].gridmax() < 3.0) ice[CONDAVG] *= 100.;
    glob = ice[CONDAVG];
    // do this to manage leap years in conditional climatology:
    nreq = ice[count].gridmax() / 2;
  #else
    if (ice[avg].gridmax() < 3.0) ice[avg] *= 100.;
    glob = ice[avg];
  #endif

  nonval = glob.gridmax();
  if (nonval <= MAXICE) nonval = 224;
  

// Mask out land and input nonvalues of all types 
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (glob[loc] > MAXICE) glob[loc] = nonval;
    if (glob[loc] < MINICE) glob[loc] = 0;
    if (glob[loc] > 100 && glob[loc] <= MAXICE) glob[loc] = 100;  

    if (land[loc] > 0 ) {
      glob[loc] = nonval;
    }
    #ifdef CONDITIONAL
      // for conditional average version:
      if (ice[count][loc] < nreq) glob[loc] = 0;
    #endif

  }
  }

  n = glob;
  s = glob;
  
  //mask out the other hemisphere for doing integrations
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
    loc.i = 0;
    ll = n.locate(loc);
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (ll.lat <= 0) n[loc] = 0;
    if (ll.lat >= 0) s[loc] = 0;
  }
  }
 
  printf("area %9.4f %9.4f %9.4f ",(float) glob.integrate(nonval)/1.e12 /100., 
             (float) n.integrate(nonval)/1.e12 /100.,
             (float) s.integrate(nonval)/1.e12 /100.  );

  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (glob[loc] > MINICE && glob[loc] <= MAXICE) {
      glob[loc] = 1;
    }
    else {glob[loc] = 0; }
    if (n[loc] > MINICE && n[loc] <= MAXICE) n[loc] = 1;
      else {n[loc] = 0; }
    if (s[loc] > MINICE && s[loc] <= MAXICE) s[loc] = 1;
      else {s[loc] = 0; }
  }
  }

  printf("extent %9.4f %9.4f %9.4f\n",(float) glob.integrate(nonval)/1.e12 , 
             (float) n.integrate(nonval)/1.e12,
             (float) s.integrate(nonval)/1.e12   );


  return 0;
}
