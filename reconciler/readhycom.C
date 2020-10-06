#include <stdio.h>
#include "grid_math.h"

#define BUFLIM 900
// Sample of how to read the hycom-formatted (.a, .b) files.  Here it is 
//  assumed that you know that .b is argv[1], and .a is argv[2] (.b being
//  the elementary descriptor, .a being the data fields

//  The variables padding and padder are used because hycom fields are normally
//  written in fortran direct access io blocked to 4096 elements.  
//    padding defines the number of extra elements needed, 
//    padder is a mvector that can be used to read in the excess.  
//  The excess domain is given values of 2^100. 

int main(int argc, char *argv[]) {
  FILE *fin, *fin2, *fout;
  int nx = 0, ny = 0, padding;
  grid2<float> lat, lon;
  mvector<float> padder;
  char line[BUFLIM];
  ijpt loc;

  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file %s\n",argv[1]);
    return 1;
  }
  fin2 = fopen(argv[2],"r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file %s\n",argv[2]);
    return 2;
  }

  fgets(line, BUFLIM/2, fin);
  sscanf(line, " %d ",&nx);

  fgets(line, BUFLIM/2, fin);
  sscanf(line, " %d ",&ny);
  fclose(fin);

  printf("nx ny = %d %d\n", nx, ny);
  fflush(stdout);

  lon.resize(nx, ny);
  lat.resize(nx, ny);
  padding = ((nx*ny+4095)/4096)*4096 - nx*ny;
  padder.resize(padding);
  printf("padding = %d\n",padding);

  lon.binin(fin2);
  padder.binin(fin2);
  lat.binin(fin2);
  padder.binin(fin2);
  printf("padder value = %f %f\n",padder[0], pow((double) 2,(double) 100) );
  fclose(fin2);

  for (loc.j = 0; loc.j < lat.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < lat.xpoints(); loc.i++) {
    printf("%3d %3d  %f %f\n",loc.i, loc.j, lat[loc], lon[loc]);
  }
  }

  fout = fopen(argv[3],"w");
  if (fout == (FILE *) NULL) {
    printf("Failed to open the output file %s\n",argv[3]);
    return 3;
  }
  lat.binout(fout);
  lon.binout(fout);
  fclose(fout); 

  return 0;
}
