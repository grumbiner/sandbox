#include <math.h>
#include "ncepgrids.h"
#include "color.h"


template <class T>
void sizer(global_ice<T> &orig) ;

int main(void) {
  FILE *fin;
  ijpt loc;
  global_ice<unsigned char> orig, newer, land, delta;
  palette<unsigned char> gg(19,65);
  
  fin = fopen("latlon.990908", "r");
  orig.binin(fin);
  fclose(fin);
  fin = fopen("globout", "r");
  newer.binin(fin);
  fclose(fin);
  fin = fopen("halfdeg.map", "r");
  land.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < orig.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < orig.xpoints(); loc.i++) {
    if (fabs(orig[loc] - newer[loc]) > 1 ) {
      printf("%3d %3d  %3d %3d  %3d\n",loc.i, loc.j, orig[loc], newer[loc], land[loc]);
    }
  }
  }

  orig.xpm("orig.xpm", 12, gg);
  newer.xpm("newer.xpm", 12, gg);
  delta = newer; delta -= orig;
  delta += (unsigned char) 128;
  delta.xpm("delta.xpm",16, gg);

  printf("Extent of orig ice\n");
  sizer(orig);
  printf("Extent of newer ice\n");
  sizer(newer);

  return 0;

}
template <class T>
void sizer(global_ice<T> &orig) {
  ijpt loc;
  global_ice<float> tmp;
  
  for (loc.j = 0; loc.j < orig.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < orig.xpoints(); loc.i++) {
    if (orig[loc] > 0 && orig[loc] < (unsigned char) 130 ) {
      tmp[loc] = 1.0;
    }
    else { 
      tmp[loc] = 0.0;
    }
  }
  }
  printf("area = %f\n",tmp.integrate() );

}
