#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  ijpt loc;
  latpt ll;
  GRIDTYPE<unsigned char> gmt, nesdis, newer;
  GRIDTYPE<float> bathy;
  int change = 0;
  palette<unsigned char> gg(19,65);
  const int gmt_coast = 0;
  const int gmt_boundary = 1;
  const int gmt_undef = 3;
  const int gmt_land = 5;
  const int gmt_ocean = 15;
  const int gmt_final_ocean = 16;
  const int gmt_water = 17;
  const int ocean = 0;
  const int land = 157;
  const int coast = 195;

  fin = fopen(argv[1],"r");
  bathy.binin(fin);
  gmt.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  nesdis.binin(fin);
  fclose(fin);
  
  newer = nesdis;
  for (loc.j = 0; loc.j < gmt.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < gmt.xpoints(); loc.i++) {
    ll = gmt.locate(loc);
    switch(nesdis[loc]) {
      case land :
          switch(gmt[loc]) {
             case gmt_water:
             case gmt_final_ocean:
             case gmt_ocean:
                printf("land vs water ");
  	        printf("%3d %3d %6.2f %7.2f  %3d %2d %7.1f\n",loc.i, loc.j,
  	           ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
                newer[loc] = ocean;
                break;
             case gmt_land:
                // do nothing, they're both land
                break;
             case gmt_undef:
                printf("undefined in gmt-derived grid\n");
                break;
             case gmt_boundary:
             case gmt_coast:
                printf("land vs bndy "); 
  	        printf("%3d %3d %6.2f %7.2f  %3d %2d %7.1f\n",loc.i, loc.j,
  	           ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
                newer[loc] = coast;
                break;
             default:
                printf("gmt flag is impossible %d %d %d\n",loc.i, loc.j, gmt[loc]);
          }
          break;
      case coast :
          switch(gmt[loc]) {
             case gmt_water:
             case gmt_final_ocean:
             case gmt_ocean:
                printf("coast vs water ");
  	        printf("%3d %3d %6.2f %7.2f  %3d %2d %7.1f\n",loc.i, loc.j,
  	           ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
                newer[loc] = ocean;
                break;
             case gmt_land:
                printf("coast vs land ");
  	        printf("%3d %3d %6.2f %7.2f  %3d %2d %7.1f\n",loc.i, loc.j,
  	           ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
                break;
             case gmt_undef:
                printf("undefined in gmt-derived grid\n");
                break;
             case gmt_boundary:
             case gmt_coast:
                // do nothing, they're both coast/boundary values
                break;
             default:
                printf("gmt flag is impossible %d %d %d\n",loc.i, loc.j, gmt[loc]);
          }
          break;
      case ocean :
          switch(gmt[loc]) {
             case gmt_water:
             case gmt_final_ocean:
             case gmt_ocean:
                // do nothing, they're both water
                break;
             case gmt_land:
                printf("ocean vs land ");
                printf("%3d %3d %6.2f %7.2f  %3d %2d %7.1f\n",loc.i, loc.j,
                   ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
                break;
             case gmt_undef:
                printf("undefined in gmt-derived grid\n");
                break;
             case gmt_boundary:
             case gmt_coast:
                printf("ocean vs coast ");
                printf("%3d %3d %6.2f %7.2f  %3d %2d %7.1f\n",loc.i, loc.j,
                   ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
                break;
             default:
                printf("gmt flag is impossible %d %d %d\n",loc.i, loc.j, gmt[loc]);
          }
          break;
      default:
          printf("impossible flag in old-form ");
          printf("%3d %3d %6.2f %7.2f  %3d %2d %7.1f\n",loc.i, loc.j,
             ll.lat, ll.lon, nesdis[loc], gmt[loc], bathy[loc] );
          switch(gmt[loc]) {
             case gmt_water:
             case gmt_final_ocean:
             case gmt_ocean:
                newer[loc] = ocean; break;
             case gmt_land:
                newer[loc] = land; break;
             case gmt_undef:
                printf("undefined in gmt-derived grid\n");
                break;
             case gmt_boundary:
             case gmt_coast:
                newer[loc] = coast; break;
             default:
                printf("gmt flag is impossible %d %d %d\n",loc.i, loc.j, gmt[loc]);
          }

      }
            
  }
  }

//  printf("%d changes\n",change);
  fin = fopen(argv[3],"w");
  newer.binout(fin);
  newer.xpm("newer.xpm",14,gg);


  return 0;
}
