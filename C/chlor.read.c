#include <stdio.h>

/*
#Program to read in a 'chlor binary file' and write it back out
#  in ascii.  Not elegant, but it works.  Takes one argument, the
#  name of the flt file.
#Program assumes that ncols, nrows, xllcorner, yllcorner, nodata_value,
#  land_mask are all constant.  
#On an HP, there is no need to swap bytes (the byteorder parameter of
#  the .hdr file
#If on some other system results become gibberish, then it will be
#  necessary to issue the command
#dd if=orig.flt of=new.flt conv=swab
# and then conduct processing on new.flt rather than orig.flt.
#Robert Grumbine 14 November 2000
*/

int main(int argc, char *argv[]) {
  int ncols = 1401, nrows = 934;
  float arry[nrows][ncols];
  float xllcorner = -81.0, yllcorner = 30.0;
  float cellsize = 0.015;
  int no_data = -999;
  int land_mask = 1000;
  float longit, latit;
  int i, j;

  FILE *fin;
  
  fin = fopen(argv[1], "r");
  fread(arry, sizeof(int), ncols*nrows, fin);
  fclose(fin);

  for (j = 0; j < nrows; j++) {
    latit = yllcorner + j*cellsize;
    for (i = 0; i < ncols; i++) {
      longit = xllcorner + i*cellsize;
      printf("%f %f  %f\n",longit, latit, arry[j][i]);
    }
  }
 

  return 0;
}
