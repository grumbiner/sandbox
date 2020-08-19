#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *inmd, *ingmt;
  GRIDTYPE<unsigned char> md, gmt, mask;
  ijpt loc;
  int undef = 2, mdland = 157, mdocean = 0, mdcoast = 195;
  int gmt_land = 5, gmt_water = 17, gmt_ocean = 15, gmt_coast = 1;
  latpt ll;

  inmd = fopen(argv[1], "r");
  md.binin(inmd);
  fclose(inmd);

  ingmt = fopen(argv[2], "r");
  gmt.binin(ingmt);
  fclose(ingmt);

  for (loc.j = 0; loc.j < md.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < md.xpoints(); loc.i++) {
    if (md[loc] == undef) {
      mask[loc] = 3;
    }
    else if ((gmt[loc] == gmt_water || gmt[loc] == gmt_ocean) && md[loc] == mdocean ) {
      ll = md.locate(loc);
      //printf("water %4d %4d  %7.3f %7.3f  %d %d \n",loc.i, loc.j, ll.lat, ll.lon, 
      //                md[loc], gmt[loc]);
      mask[loc] = 1;
    }
    else if (md[loc] == mdland && (gmt[loc] == gmt_land) ) {
      ll = md.locate(loc);
      //printf("land %4d %4d  %7.3f %7.3f  %d %d \n",loc.i, loc.j, ll.lat, ll.lon, 
      //                md[loc], gmt[loc]);
      mask[loc] = 0;
    }
    else if (md[loc] == mdcoast && (gmt[loc] == gmt_coast) ) {
      ll = md.locate(loc);
      //printf("coast %4d %4d  %7.3f %7.3f  %d %d \n",loc.i, loc.j, ll.lat, ll.lon, 
      //                md[loc], gmt[loc]);
      mask[loc] = 0;
    }
    else if (md[loc] == gmt[loc]) {
      ll = md.locate(loc);
      //printf("other= %4d %4d  %7.3f %7.3f  %d %d \n",loc.i, loc.j, ll.lat, ll.lon, 
      //                md[loc], gmt[loc]);
      mask[loc] = 2;
    }
    else if (md[loc] != gmt[loc]) {
      ll = md.locate(loc);
      printf("conflict %4d %4d  %7.3f %7.3f  %d %d \n",loc.i, loc.j, ll.lat, ll.lon, 
                      md[loc], gmt[loc]);
      mask[loc] = 4;
    }

  }
  }

  palette<unsigned char> cols(5);
  cols.set_color(0, 0, 0, 0);   // both land
  cols.set_color(1, 0, 0, 255); // both water
  cols.set_color(2, 255, 255, 255); //other equal
  cols.set_color(3, 255, 0, 0); //undef
  cols.set_color(4, 0, 255, 0); // nonequal
  mask.xpm("diff.xpm",1,cols);


  return 0;
}
