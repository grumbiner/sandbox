#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  ijpt loc;
  latpt ll;
  GRIDTYPE<unsigned char> gmt, nesdis;
  GRIDTYPE<float> bathy;
  int change = 0;
  palette<unsigned char> gg(19,65);
  unsigned char gmt_coast = 0;
  unsigned char gmt_boundary = 1;
  unsigned char gmt_undef = 3;
  unsigned char gmt_land = 5;
  unsigned char gmt_ocean = 15;
  unsigned char gmt_final_ocean = 16;
  unsigned char gmt_water = 17;
  unsigned char ocean = 0;
  unsigned char land = 157;
  unsigned char coast = 195;

  fin = fopen(argv[1],"r");
  bathy.binin(fin);
  gmt.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  nesdis.binin(fin);
  fclose(fin);
  
  for (loc.j = 0; loc.j < gmt.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < gmt.xpoints(); loc.i++) {
    ll = gmt.locate(loc);
    if (nesdis[loc] == ocean) {
      if (gmt[loc] != gmt_ocean && gmt[loc] != gmt_final_ocean &&
  	gmt[loc] != gmt_water) {
  	    printf("%3d %3d %6.2f %7.2f  %3d %2d %7.1f\n",loc.i, loc.j,
  	       ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
        nesdis[loc] = coast;
        change += 1;
      }
    }
    else if (nesdis[loc] == land) {
      if (gmt[loc] != gmt_land) {
	   printf("%3d %3d %6.2f %7.2f  %3d %2d %7.1f\n",loc.i, loc.j,
	     ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
      }
      if (gmt[loc] == gmt_water || gmt[loc] == gmt_ocean) {
        nesdis[loc] = coast;
        change += 1;
      }
    }
    else if (nesdis[loc] == coast && gmt[loc] != gmt_boundary) {
        printf("%3d %3d %6.2f %7.2f  %3d %2d  %7.1f\n",loc.i, loc.j,
           ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
        if (gmt[loc] == land) {
          nesdis[loc] = land; 
          change += 1;
        }
    }
    else {
        printf("%3d %3d %6.2f %7.2f  %3d %2d  %7.1f illegal\n",loc.i, loc.j,
           ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
        if (gmt[loc] == gmt_land) {
          nesdis[loc] = land;
          change += 1;
        }
        else if (gmt[loc] == gmt_ocean) {
          nesdis[loc] = ocean;
          change += 1;
        }
    }
//  Now prep for graphics
    if (nesdis[loc] == land) {
      nesdis[loc] = gmt_land;
    }
    else if (nesdis[loc] == coast) {
      nesdis[loc] = gmt_coast;
    }
    else if (nesdis[loc] == ocean) {
      nesdis[loc] = gmt_final_ocean;
    }
    else {
      printf("illegal value %d\n",nesdis[loc]);
    }
  }
  }

  nesdis.xpm("update.xpm",1,gg);
  printf("%d changes\n",change);

  return 0;
}
