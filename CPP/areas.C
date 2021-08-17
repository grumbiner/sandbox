#include <stdio.h>

// Program template to construct xpm files from unsigned char inputs
// Add in production of a global area 

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  GRIDTYPE<float> in, yland; 
  GRIDTYPE<unsigned char> land;
  GRIDTYPE<float> forarea;
  palette<unsigned char> gg(19, 65);
  ijpt loc;
  float area, extent, concentration;

  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open input file\n");
    return 1;
  }
  in.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open input file\n");
    return 1;
  }
  land.binin(fin); 
  fclose(fin);

  if (in.average() < 5) in *= 100.;
  conv(land, yland);
  conv(in, forarea);

//Compute the area from the percentage cover, no check on over values
  for (loc.j = 0; loc.j < forarea.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < forarea.xpoints(); loc.i++) {
    if (yland[loc] == 0.) {
      forarea[loc] = 0.;
      in[loc] = 0.;
    }
    if (forarea[loc] < 15 || forarea[loc] > 128 ) {
      forarea[loc] = 0.0;
      in[loc] = 0.;
    }
 
  }
  }
  area = forarea.integrate()/1.e12/100.;

//Compute the ice extent
  for (loc.j = 0; loc.j < forarea.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < forarea.xpoints(); loc.i++) {
    if (forarea[loc] >= 15 && forarea[loc] <= 128 ) {
      forarea[loc] = 1.0;
    }
    else {
      forarea[loc] = 0.0;
    }
  }
  }
  extent = forarea.integrate()/1.e12;
  concentration =  area / extent; 
  printf("%s %6.3f %6.3f %5.3f\n",argv[3], area, extent, concentration);

  return 0;
}
