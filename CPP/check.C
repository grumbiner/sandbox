#include "ncepgrids.h"
#include "resops.h"

#define NX 300
#define NY 421
#define RATIO 4

int main(int argc, char *argv[]) {
  grid2<float> bathy_low(NX, NY), bathy_high(NX*RATIO, NY*RATIO);
  grid2<unsigned char> mask_low(NX, NY), mask_high(NX*RATIO, NY*RATIO);
  hycom<unsigned char> low;
  FILE *fin1, *fin2;

  ijpt loc1, loc2;
  unsigned char boundary = 1, land = 5, water = 17, ocean = 15, undef = 3;
  unsigned char paved = 13, final_ocean = 16;
  int count;
  float avger;
  latpt ll;
  
  fin1 = fopen(argv[1],"r");
  fin2 = fopen(argv[2], "r");
  bathy_low.binin(fin1);
  mask_low.binin(fin1);
  bathy_high.binin(fin2);
  mask_high.binin(fin2);
  fclose(fin1);
  fclose(fin2);

//  printf("max, min %f %f\n",(float) mask_low.gridmax(), 
//                            (float) mask_low.gridmin() );

  for (loc1.j = 0; loc1.j < mask_low.ypoints(); loc1.j++) {
  for (loc1.i = 0; loc1.i < mask_low.xpoints(); loc1.i++) {
    if (mask_low[loc1] == final_ocean) {
      count = 0;
      for (loc2.j = loc1.j*RATIO; loc2.j < loc1.j*RATIO + RATIO; loc2.j++) {
      for (loc2.i = loc1.i*RATIO; loc2.i < loc1.i*RATIO + RATIO; loc2.i++) {
        if (mask_high[loc2] == final_ocean) count += 1;
      }
      }
      if (count == 0) {
        ll = low.locate(loc1);
        printf("low res is ocean, but no high res point is at %d %d  %f %f  %2d %f\n",
                 loc1.i, loc1.j, ll.lon, ll.lat, 
                 (int) mask_low[loc1], bathy_low[loc1]);
        for (loc2.j = loc1.j*RATIO; loc2.j < loc1.j*RATIO + RATIO; loc2.j++) {
        for (loc2.i = loc1.i*RATIO; loc2.i < loc1.i*RATIO + RATIO; loc2.i++) {
          printf("  %d %d  %2d %f\n",loc2.i, loc2.j, (int)mask_high[loc2],
                     bathy_high[loc2] );
        }
        }
      }
    }

    avger = 0.;
    for (loc2.j = loc1.j*RATIO; loc2.j < loc1.j*RATIO + RATIO; loc2.j++) {
    for (loc2.i = loc1.i*RATIO; loc2.i < loc1.i*RATIO + RATIO; loc2.i++) {
      avger += bathy_high[loc2];
    }
    }
    avger /= RATIO*RATIO;
    if (fabs(avger - bathy_low[loc1])/min(bathy_low[loc1],avger) > 0.1) { 
      printf("all %d %d  %f %f  %f\n",loc1.i, loc1.j, bathy_low[loc1], avger,
          fabs(avger - bathy_low[loc1])/min(bathy_low[loc1],avger) );
    }

  }
  }

  return 0;
}             
