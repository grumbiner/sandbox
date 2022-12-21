// Read in an unformatted grid of GRIDTYPE<VARTYPE> and extract
//   info at a specified (arg) lat-long
// Robert Grumbine  Jun 28  1999

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include "ncepgrids.h"

using namespace std;

int main(int argc, char *argv[]) {
  GRIDTYPE<VARTYPE> x;
  char **xxx = NULL;
  FILE *fin;
  latpt loc;
  fijpt floc;

  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL) {
    cout << "Failed to open " << argv[1] << endl;
    return 1;
  }
  loc.lat = (float) strtod(argv[2], xxx);
  loc.lon = (float) strtod(argv[3], xxx);

  x.binin(fin);
  floc = x.locate(loc);
  
  cout << "value at " << loc.lat << " " << loc.lon << " " ;
  cout << (float) x[floc] << endl;  
  cout << "grid point i j "; cout << floc.i << "  " << floc.j << endl;
  cout.flush();

  return 0;
} 
