#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northgrid<float> north;
  southgrid<float> south;
  vector<float> nvec(north.xpoints());
  vector<float> svec(south.xpoints());
  ijpt pt1, pt2;
  int j;
  FILE *fin1, *fin2;
  FILE *fout;
  char fname[800];
  float fref=2.24;
  float scale;

// Open the ice analyses and initialize the data
  fin1 = fopen("north","r");
  if (fin1 == (FILE *) NULL) {
    printf("No file for today\n");
    north.set(fref);
  }
  else {
    north.binin(fin1);
  }

  fin2 = fopen("south","r");
  if (fin2 == (FILE *) NULL) {
    printf("No file for today\n");
    south.set(fref);
  }
  else {
    south.binin(fin2);
  }


// Important: Ensure that the scalings are the same.  The archive needs to
//   be made consistent in this way!!
  if (north.average() > 10.0 && south.average() > 10.0 ) {
    scale = 100.;
    north /= 100.;
    south /= 100.;
  }
  else if (north.average() < 2.56 && south.average() < 2.56 ) {
    scale = 1.0;
  }
  else {
    printf("hit an impossible scaling\n");
    printf("averages for north and south are %f and %f\n", north.average(),
             south.average() );
    return -1;
  }
    
 
// Loop over j (y), appending data to the end of the files
  pt1.i = 0;
  pt2.i = north.xpoints() - 1;
  for (j = 0; j < north.ypoints() ; j++) {
    pt1.j = j;
    pt2.j = j;
    north.get_transect(pt1, pt2, nvec);
    sprintf(fname,"n.%03d\0",j);
    fout = fopen(fname, "a");
    if (fout == (FILE *) NULL) {
      printf("Failed to open the output file, exiting\n");
      printf("name = %s\n", fname);
      return -1;
    }
    nvec.binout(fout);
    fclose(fout);
  }

  pt1.i = 0;
  pt2.i = south.xpoints() - 1;
  for (j = 0; j < south.ypoints() ; j++) {
    pt1.j = j;
    pt2.j = j;
    south.get_transect(pt1, pt2, svec);
    sprintf(fname,"s.%03d\0",j);
    fout = fopen(fname, "a");
    if (fout == (FILE *) NULL) {
      printf("Failed to open the output file, exiting\n");
      printf("name = %s\n", fname);
      return -1;
    }
    svec.binout(fout);
    fclose(fout);
  }

  return 0;
}
