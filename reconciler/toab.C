#include <stdio.h>
#include "resops.h"

// Perform a very simple-minded translation of the mask file to the .a, .b
//   format.
// Robert Grumbine 19 August 2003

int main(int argc, char *argv[]) {
  FILE *fin, *fouta, *foutb;
  hycom<unsigned char> field;
  hycom<float> bathy;
  ijpt loc;

  fin = fopen(argv[1],"r");
  fouta = fopen(argv[2],"w");
  foutb = fopen(argv[3],"w");

  bathy.binin(fin);
  field.binin(fin);

  for (loc.j = 0; loc.j < field.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < field.xpoints(); loc.i++) {
    bathy[loc] = field[loc] ;
  }
  }

  bathy.outa(fouta);
  fclose(fouta);

  bathy.outb(foutb);
  fclose(foutb);

  return 0;
} 
