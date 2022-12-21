#include <stdio.h>

#include "ncepgrids.h"

// Convert from the initial grid to some output grid that is 
//  and N:1 reduction
int main(void) {
  INTYPE<unsigned char> iland;
  OUTTYPE<unsigned char> oland;
  FILE *fin, *fout;
  ijpt loc;

  fin = fopen("landin","r");
  fout = fopen("landout","w");

  iland.binin(fin);
  oland.reduce(iland);
  oland.binout(fout);

  return 0;
}
