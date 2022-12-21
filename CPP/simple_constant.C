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
  VARTYPE tmp;
  fijpt floc;

  fin = fopen(argv[1],"w");
  if (fin == (FILE *) NULL) {
    cout << "Failed to open " << argv[1] << endl;
    return 1;
  }
  tmp = (VARTYPE) strtod(argv[2], xxx);
  x.set(tmp);

  x.binout(fin);
  fclose(fin);
  
  return 0;
} 
